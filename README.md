# stash

`stash` is a command line program for storing text data in encrypted form.

It uses [sqlite](https://sqlite.org/) as its database.

All user data is encrypted using [AES 256](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) cipher.

Data is hashed for indexing purposes using a good random salt + [SHA512](https://en.wikipedia.org/wiki/SHA-2).

## Getting Started

Run the following command in a directory where we want to store your stash.

```
stash init
```

This will create a `.stash` directory and initialize its `sqlite` database.

We will be prompted for the encryption-key (password) we wish to use for the stash. The key is not saved anywhere, but a
salted hash (good random salt + SHA512) of the key and the salt is saved in your `.stash` directory.

When we run a stash command, we will be prompted for our encryption key. It is checked against the hash stored during
`stash init`. For the duration of the command, the encryption key will be used for encrypting/decrypting data.

The salt stored during `stash init` is also used for hashing any data for indexing purposes.

If we wish to keep a global stash, we can set `STASH_DIRECTORY` environment variable. Running
the above command with `STASH_DIRECTORY` set will initialize stash in the set directory.

## Browsing

### Terminal user interface (default)

```
stash browse
```

| Shortcuts              | Description                                      |
|------------------------|--------------------------------------------------|
| ESC (q)                | Quit                                             |
| ?                      | Show this help                                   |
| +                      | Add a new key                                    |
| >                      | Add a child key to selected key                  |
| -                      | Delete selected key                              |
| , (r)                  | Rename selected key                              |
| /                      | Search and sort by pattern                       |
| y                      | Copy value of selected key into system clipboard |
| Enter                  | Set value of selected key                        |
| H                      | See history of values of selected key            |
| Left arrow (h)         | Move to parent of selected key                   |
| Right arrow (l)        | Move to child of selected key                    |
| Up arrow (k, Ctrl-p)   | Select above                                     |
| Down arrow (j, Ctrl-n) | Select below                                     |
| g, G                   | Move to top, Move to bottom                      |
| Ctrl-u, Ctrl-d         | Scroll keys half page up / down                  |
| Ctrl-b, Ctrl-f         | Scroll keys one page up / down                   |

### As Markdown text

```
stash browse -f markdown
```

Opens as markdown text in default editor. Set EDITOR environment variable to your editor command. Defaults to vim.

### As Org text

```
stash browse -f org
```

Opens as Org text in default editor. Set EDITOR environment variable to your editor command. Defaults to vim.

## Dumping contents to stdout

### As Markdown text

```
stash dump -f markdown
```

### As Org text

```
stash dump -f org
```

## Shell Autocompletion

### bash

`source` output from following command

```
stash --bash-completion-script `which stash`
```

### zsh

Create a `_stash` in your $FPATH (try `~/.zsh/_stash`) with contents from following command

```
stash --zsh-completion-script `which stash`
```

### fish

`source` output from following command

```
stash --fish-completion-script `which stash`
```
