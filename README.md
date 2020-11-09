# stash

`stash` is a command line program for storing text data in encrypted form.

It uses [sqlite](https://sqlite.org/) as its database. All user data is encrypted using [AES 256](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) cipher.

## Getting Started

Run the following commnad in a directory where we want to store your stash.

```
stash init
```

This will create a `.stash` directory and initialize its `sqlite` database. We will also be
prompted for the encryption-key (password) we wish to use for the stash. The key is not saved anywhere, but a
salted hash (good random salt + SHA512) of the key and the salt used is saved in your `.stash` directory. The hash is
checked with entered encryption-key when we run stash commands -- this ensures we don't accidentally use
different encryption-key to store data. The salt is also used to hash our data -- to speed
up searches and to check for changes.

If we wish to keep a global stash, we can set `STASH_DIRECTORY` environemnt variable. Running
the above command with `STASH_DIRECTORY` set will initialize stash in the set directory.
## Browsing

### Terminal user interface (default)

```
stash browse
```

### As Markdown text

```
stash browse -f markdown
```

### As Org text

```
stash browse -f org
```

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
