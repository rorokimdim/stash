# Demo

Symlink this directory in your quicklisp projects folder, which is probably `~/quicklisp/local-projects` or `~/.roswell/lisp/quicklisp/local-projects`:

```
cd {path-to-your-quicklisp-projects-folder}
ln -s {path-to-this-folder} stash
```


In a repl,

```lisp
(ql:quickload :stash)
(in-package :stash)
(stash/start)
```
