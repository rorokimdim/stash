import atexit
import shutil
import json
import subprocess
import sys
import uuid

import bcoding
import edn_format
import tabulate

# If the pod command is not in your PATH,
# set this to the full path of the pod command.
POD_COMMAND = shutil.which('stash')

POD_NAMESPACE = "pod.rorokimdim.stash"
POD_NAME = "stash"

POD_PROCESS = None


class InvokeException(Exception):
    pass


def get_pod():
    """Gets pod process."""
    if not POD_COMMAND:
        print('☠️  pod executable command not found.\nIf it is not in your PATH, set POD_COMMAND to its full path.')
        sys.exit(1)

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
    """Sets up the repl."""
    print(f"{POD_NAME} v{pod_invoke('version')}")
    load_pod_functions()

    print(f'▸ Available functions from {POD_NAME}')
    print('  Use help function for more information.')
    pod_functions = [fn for name, fn in globals().items()
                     if callable(fn) and name.startswith(f'{POD_NAME}_')]
    print(tabulate_functions(pod_functions))
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


def tabulate_functions(fns):
    return tabulate.tabulate(
        [(fn.__name__, fn.__doc__.split('\n')[0]) for fn in fns],
        headers=['name', 'description'],
        tablefmt='psql',
    )


def load_pod_functions():
    """Loads pod functions in module scope."""
    def sanitize_arglists(xs):
        if len(xs) == 0:
            return '[]'
        elif len(xs) == 1:
            return f'[{", ".join(xs[0])}]'
        else:
            return ', '.join(sanitize_arglists([x]) for x in xs)

    def load(name, docstring, arglists):
        def fn(*args):
            return pod_invoke(name, *args)

        fn.__name__ = f"{POD_NAME}_{name.replace('-', '_')}"

        doc_lines = [x.strip() for x in docstring.split('\n')]
        args = sanitize_arglists(arglists).replace('&, ', '*')
        if len(arglists) == 1:
            doc_lines.append(f'\nargs must be {args}')
        else:
            doc_lines.append(f'\nargs must be one of {args}')

        fn.__doc__ = '\n'.join(doc_lines)
        globals()[fn.__name__] = fn

    pod = get_pod()
    write(pod, dict(id=f'describe-{uuid.uuid4().hex}', op='describe'))
    description = read(pod)
    for x in description['namespaces'][0]['vars']:
        name = x['name']
        meta = edn_format.loads(x.get('meta', '{}'))
        m = {'name': name}
        for k, v in meta.items():
            if isinstance(v, (tuple, list)):
                m[k.name] = [[x.name for x in xs] for xs in v]
            elif isinstance(v, str):
                m[k.name] = v
        load(m['name'],
             m.get('doc', 'No docstring provided'),
             m.get('arglists', []))


def pod_invoke(name, *args):
    """Invokes a pod function by name."""
    pod = get_pod()
    write(pod, dict(
        op='invoke',
        id=f'{name}-{uuid.uuid4().hex}',
        var=f'{POD_NAMESPACE}/{name}',
        args=json.dumps(args)))

    result = read(pod)

    if 'ex-message' in result:
        raise InvokeException(f"{result['ex-message']}\n{result.get('ex-data')}")

    return json.loads(result['value'])


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

    return inner(stash_tree(parent_id=parent_id))  # noqa


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


startup()
