# mhv2109's emacs config

Based on flyingmachine's `emacs-for-clojure` repository found [here](https://github.com/flyingmachine/emacs-for-clojure).

Updated with suggestions from aaronbedra's configuration found [here](http://aaronbedra.com/emacs.d/#languages).

## Languages & Tooling
### Python
Python configurations base on [this configuration](https://realpython.com/emacs-the-best-python-editor/).
For python-mode to work correctly, the following packages are necessary:
```
$ pip install --upgrade flake8 autopep8 ipython rope yapf black
```
Also install [pyenv](https://github.com/pyenv/pyenv) and [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv).
Elpy docs are [here](https://elpy.readthedocs.io/en/latest/).

### Go
Go configurations based on [this configuration](https://johnsogg.github.io/emacs-golang).
For go-mode to work correctly, the following packages are necessary:
```
$ go get -u golang.org/x/tools/cmd/...
$ go get -u github.com/rogpeppe/godef/...
$ go get -u github.com/nsf/gocode
$ go get -u golang.org/x/tools/cmd/goimports
```

### LeetCode
See instructions for installing LeetCode plugin dependencies [here](https://github.com/ginqi7/leetcode-emacs/tree/8624496af9e105c3e07d88844b37ede87540d604).
LeetCode settings can be modified in [customizations/leetcode.el](./customizations/leetcode.el). By default, the LeetCode workspace is set as
`~/workspace/leetcode` and the language is set to `python3`.

## Usage
Clone repository using `git clone --recurse-submodules https://github.com/mhv2109/.emacs.d.git` into `~/.emacs.d` and start emacs.

