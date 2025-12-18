# AhogeEmacs

My emacs configuration.

## Installation

```bash
cd ~
rm -r ~/.emacs.d
git clone https://github.com/BlockyDeer/.emacs.d
cd ~/.emacs.d/
git submodule init
git submodule update
```

Then open your emacs:

```bash
emacs
```

## Note

the `ans-mode.el` in `~/.emacs.d` is not useful for common programming (I use it for some my own situations), you can remove it by the following steps:

1. Delete the file `ans-mode.el` in `~/.emacs.d`.
2. Remove the line `(load-el "ans-mode.el")` in the `init.el`.
   
