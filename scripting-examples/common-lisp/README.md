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

Try these [functions](https://github.com/rorokimdim/stash/blob/59d625a7a881a25a004b2a6b4a612603324b6a67/scripting-examples/common-lisp/stash.lisp#L97-L187).
