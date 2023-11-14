# mhv2109's emacs config v2

## Usage
Clone repository using `git clone --recurse-submodules https://github.com/mhv2109/.emacs.d.git` into `~/.emacs.d` and start emacs.

## Getting Help

### Info Manual

* `M-x info` or `C-h i`: Open Info Manual
  * `[` and `]`: Previous/next node
  * `l` and `r`: Back/forward in history
  * `n` and `p`: Previous/next sibling node
  * `u`: Up one level to parent node
  * `SPC`: Scroll one screen at a time
  * `TAB`: Cycle through cross-references and links
  * `RET`: Open active link
  * `m`: Prompt for menu link and open it
  * `q`: Close info browser

#### Usefule Pages

  * `C-h i RET m Emacs Lisp Intro RET`: Introduction to Elisp

### Apropos

Search for symbols whose names match a pattern. All commands support regex.

* `M-x apropos`: All symbols (i.e. everything)
* `M-x apropos-command` or `C-h a`: Commands (not functions)
* `M-x apropos-documentation` or `C-h d`: Doc strings
* `M-x apropos-library`: All variables and functions defined in a library
* `M-x apropos-value`: All symbols with a particular value

### Describe

Display the full documentation of symbols.

* `M-x describe-mode` or `C-h m`: Major-mode of current buffer
* `M-x describe-command` or `C-h x`: Commands (not functions)
* `M-x describe-function` or `C-h f`: Functions
* `M-x describe-variable` or `C-h v`: Variables
* `M-x describe-key` or `C-h k`: Key bindings

#### Describe Buffer Shortcuts

* `i`: Open Info manual
* `s`: Jump to source definition
