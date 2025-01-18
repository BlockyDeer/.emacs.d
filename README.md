# AhogeEmacs

My emacs configuration. It's very weak but it works.

## Installation

```bash
cd ~
rm -r ~/.emacs.d
git clone https://github.com/BlockyDeer/.emacs.d
```

Then open your emacs:

```bash
emacs
```

## Note

the `ans-mode.el` in `~/.emacs.d` is not useful for common programming (I use it for some my own situations), you can remove it by the following steps:

1. Delete the file `ans-mode.el` in `~/.emacs.d`.
2. Remove the line `(load (expand-file-name "ans-mode.el" user-emacs-directory))` in the `init.el`.
   
