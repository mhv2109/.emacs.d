
## mhv2109's emacs config

Based on flyingmachine's `emacs-for-clojure` repository found [here](https://github.com/flyingmachine/emacs-for-clojure).

Updated with suggestions from aaronbedra's configuration found [here](http://aaronbedra.com/emacs.d/#languages).

Go configurations based on [this configuration](https://johnsogg.github.io/emacs-golang).
For go-mode to work correctly, the following packages are necessary:
```
$ go get -u golang.org/x/tools/cmd/...
$ go get -u github.com/rogpeppe/godef/...
$ go get -u github.com/nsf/gocode
$ go get -u golang.org/x/tools/cmd/goimports
```

Python configurations base on [this configuration](https://realpython.com/emacs-the-best-python-editor/).
For python-mode to work correctly, the following packages are necessary:
```
$ pip install --upgrade flake8 autopep8 ipython rope yapf black
```
Also install [pyenv](https://github.com/pyenv/pyenv) and [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv).
Elpy docs are [here](https://elpy.readthedocs.io/en/latest/).


## Usage
Clone repository into `~/.emacs.d` and start emacs.