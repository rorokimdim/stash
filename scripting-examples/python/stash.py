import atexit
import inspect
import json
import os
import subprocess
import sys
import uuid

import bcoding

POD_COMMAND = os.environ['STASH_COMMAND_FULL_PATH']
POD_PROCESS = None

STASH_FILE_PATH = "demo.stash"


class InvokeException(Exception):
    pass


def get_pod():
    """Gets pod process."""
    global POD_PROCESS
    if not POD_PROCESS:
        POD_PROCESS = subprocess.Popen(
            [POD_COMMAND],
            env=dict(BABASHKA_POD='true'),
            stdin=subprocess.PIPE,
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE)
    return POD_PROCESS


def startup():
    """Prepares to talk to stash."""
    if stash_init():
        print('✔︎ stash initialized.')
        print('Try these functions:')
        print('\n'.join(f'▸ {name}' for name, _ in get_stash_functions()))
    else:
        print(f'☠️  Invalid encryption key for "{STASH_FILE_PATH}". Failed to initialize stash.')
        print(f'\nPlease delete "{STASH_FILE_PATH}" to create one with a different key.')

    atexit.register(shutdown)


def shutdown():
    """Shuts down pod process."""
    print('Shutting down pod...')
    pod = get_pod()

    try:
        write(pod, dict(op='shutdown'))
    except ValueError as e:
        if 'closed file' in str(e):
            return
        raise

    pod.stdin.close()
    pod.terminate()
    pod.wait(timeout=0.2)

    global POD_PROCESS
    POD_PROCESS = None


def write(pod, data):
    """Writes data to pod's stdin."""
    pod.stdin.write(bcoding.bencode(data))
    pod.stdin.flush()


def read(pod):
    """Reads data from pod's stdout."""
    return bcoding.bdecode(pod.stdout)


def get_description():
    """Gets pod description."""
    pod = get_pod()

    write(pod, dict(op='describe'))
    return read(pod)


def get_stash_functions():
    """Gets a list of tuples of (stash-function-name, docstring)."""
    return sorted((name, x.__doc__) for name, x in inspect.getmembers(sys.modules[__name__])
                  if name.startswith('stash_')
                  if callable(x))


def stash_help():
    """Print some help on stash functions."""
    for name, doc in get_stash_functions():
        print(f'▸ {name}')
        print('\n'.join(line.strip() for line in doc.splitlines()))
        print()


def stash_init():
    """Initializes stash.

    The encryption key is read from STASH_ENCRYPTION_KEY environment variable.

    If `STASH_FILE_PATH` does not exist, it will be created.
    """
    return stash_invoke(
        'init',
        {'encryption-key': os.environ['STASH_ENCRYPTION_KEY'],
         'stash-path': STASH_FILE_PATH,
         'create-stash-if-missing': True})


def stash_nodes(parent_id=0):
    """Gets all nodes stored in stash.

    If a parent-node-id is provided, only nodes with that parent-id are returned.
    """
    return stash_invoke('nodes', parent_id)


def stash_tree(parent_id=0):
    """Gets all nodes stored in stash as a tree.

    Returns a dict of the form {key: [node-id, value, child-tree]}.

    If a parent-node-id is provided, only nodes with that parent-id are returned.
    """
    return stash_invoke('tree', parent_id)


def stash_tree_on_id(parent_id=0):
    """Gets all nodes stored in stash as a tree indexed by node-ids.

    Returns a dict of the form {id: {"key": key, "value": value "children": child-tree}}.

    If a parent-node-id is provided, only nodes with that parent-id are returned.
    """
    def inner(stree):
        tree = {}
        for k, (nid, value, child_stree) in stree.items():
            tree[nid] = dict(key=k, value=value, children=inner(child_stree))
        return tree

    return inner(stash_tree(parent_id=parent_id))


def stash_tree_on_id_to_paths(tree_on_id):
    """Gets paths to nodes in a tree-on-id data-structure.

    See stash_tree_on_id function.
    """
    paths = {}

    def inner(t, pid):
        for k, v in t.items():
            paths[k] = paths.get(pid, []) + [k]
            children = v['children']
            if children:
                inner(children, k)

    inner(tree_on_id, 0)
    return paths


def stash_node_versions(node_id):
    """Gets all version of a node.

    stash currently only keeps upto 10 versions.
    """
    return stash_invoke('node-versions', node_id)


def stash_get(*keys):
    """Gets value corresponding to a path of keys."""
    return stash_invoke('get', *keys)


def stash_keys(*parent_ids):
    """Gets keys under provided parent-ids.

    The root parent-id is 0.
    """
    return stash_invoke('keys', *parent_ids)


def stash_set(keys, value):
    """Sets value of a path of keys."""
    return stash_invoke('set', *(keys + [value]))


def stash_add(parent_id, key, value):
    """Adds a new node under a parent."""
    return stash_invoke('add', parent_id, key, value)


def stash_update(node_id, value):
    """Updates a node's value."""
    return stash_invoke('update', node_id, value)


def stash_delete(*node_ids):
    """Deletes nodes by ids."""
    return stash_invoke('delete', *node_ids)


def stash_invoke(name, *args):
    """Invokes a stash command by name."""
    pod = get_pod()
    write(pod, dict(
        op='invoke',
        id=f'{name}-{uuid.uuid4().hex}',
        var=f'pod.rorokimdim.stash/{name}',
        args=json.dumps(args)))

    result = read(pod)

    if 'ex-message' in result:
        raise InvokeException(result['ex-message'])

    return json.loads(result['value'])


def stash_browse():
    """Launches stash terminal-ui."""
    subprocess.Popen([POD_COMMAND, 'browse', STASH_FILE_PATH]).wait()


startup()
