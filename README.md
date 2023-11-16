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

## Key bindings

### Basics

* `M-x find-file` or `C-x C-f`: Open file
* `M-x save-buffer` or `C-x C-s`: Save buffer
* `M-x switch-to-buffer` or `C-x b`: Switch buffer
* `M-x kill-buffer` or `C-x k`: Kill (close) buffer
* `M-x list-buffers` or `C-x C-b`: List all open buffers
* `M-x save-buffers-kill-terminal` or `C-x C-c`: Exit Emacs
* `M-x keyboard-escape-quit` or `ESC ESC ESC`: Quit out of regions, prefix arguments, prompts and returns to just one window
* `M-x undo` or `C-x u` or `C-/`: Undo changes
* `F10`: Display menu bar

### Window Management

* `M-x delete-window` or `C-x 0`: Delete current window
* `M-x delete-other-windows` or `C-x 1`: Delete other windows
* `M-x split-window-below` or `C-x 2`: Split window below
* `M-x split-window-right` or `C-x 3`: Split window right
* `M-x other-window` or `C-x o`: Switch active window

I also have `windmove` enabled, so can also switch windows directionally with `S-<left>`, `S-<right>`, `S-<up>`, `S-<down>`.

### Elemental Movement

* `<left>, <right>, <up>, <down>`: Move by character in all directions
* `C-f`: Move forward by character
* `C-b`: Move backward by character
* `C-p`: Move to previous line
* `C-n`: Move to next line

#### By Word

* `M-f` or `ESC <right>`: Move forward by word
* `M-b` or `ESC <left>`: Move backward by word

#### By Line

* `C-a`: Move to beginning of line
* `C-e`: Move to end of line
* `M-m`: Move to first non-whitespace character on the line
* `M-g M-g`: Go to line

#### By sexp, List, and Balanced Expression

* `C-M-f`: Move forward one sexp
* `C-M-b`: Move backward one sexp
* `C-M-d`: Move down into a list
* `C-M-u`: Move up out of a list
* `C-M-n`: Move forward to the next list
* `C-M-p`: Move backward to the previous list

#### By Function

* `C-M-a`: Move to beginning of function definition
* `C-M-e`: Move to end of function definition

#### Scrolling

* `C-v`: Scroll down
* `M-v`: Scroll up
* `C-M-v`: Scroll other window down
* `C-- C-M-v`: Scroll other window up
* `M-<`: Scroll to beginning of buffer
* `M->`: Scroll to end of buffer

#### Regions and Selections

* `C-<SPC>`: Set the mark and toggle region
  * `C-<SPC> C-<SPC>`: Effectively just sets the mark
* `C-u C-<SPC>`: Jump to the mark
  * Repeated calls jumps to previous marks on the mark-ring
* `C-x C-x`: Swap point and mark
* `M-h`: Mark the next paragraph
* `C-x h`: Mark the whole buffer
* `C-M-h`: Mark the next defun
* `M-@`: Mark the next word
* `C-M-<SPC>` or `C-M-@`: Mark next sexp

#### Bookmarks and Registers

* `C-x r m`: Set a bookmark
* `C-x r l`: List bookmarks
* `C-x r b`: Jump to bookmark

* `C-x r n`: Store number in register
* `C-x r s`: Store selection in register
* `C-x r <SPC>`: Store point in register
* `C-x r +`: Increment number in register
* `C-x r j`: Jump to register
* `C-x r i`: Insert content of register

#### Searching and Indexing

* `C-s` or `C-r`: Forward/reverse isearch
  * Press repeatedly to repeat for previous term
* `C-M-s` or `C-M-r`: Forward/reverse regex isearch
* `RET`: Pick selected match
* `C-g`: Exit isearch
