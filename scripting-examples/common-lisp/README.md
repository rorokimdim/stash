# Demo

Symlink this directory to quicklisp projects folder.

```
ln -s {path-to-this-folder} stash
```

In a repl,

```lisp
(ql:quickload :stash)
(in-package :stash)
```

Pretty print documentatin for available functions:

```lisp
(stash/help)
```

Initialize with sample stash file:

```lisp
(stash/init)
```
