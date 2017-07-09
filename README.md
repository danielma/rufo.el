# rufo.el [![Build Status](https://travis-ci.org/danielma/rufo-mode.el.svg?branch=master)](https://travis-ci.org/danielma/rufo-mode.el)

TODO: add to melpa

Minor-mode to automatically format ruby with [rufo][].

## Usage

Ensure that you have [rufo][] installed and in your path.

Then, in your `init.el`:

```elisp
(add-hook 'ruby-mode 'rufo-minor-mode)
```

## Options

`rufo-mode` has first-class support for using rufo with bundler

```elisp
(setq rufo-minor-mode-use-bundler t)
```

You can change the location of [rufo][]. 

```elisp
(setq rufo-minor-mode-executable "rufa")
```

You can enable `rufo-minor-mode-debug-mode` which will output additional info into the messages buffer

## Thanks

* [@aaronjensen][] for [eslintd-fix][], which I forked for this project
* The authors of [go-mode.el][], from which I copied the RCS diff application code

[rufo]: https://github.com/asterite/rufo
[eslintd-fix]: https://github.com/aaronjensen/eslintd-fix
[@aaronjensen]: https://github.com/aaronjensen
[go-mode.el]: https://github.com/dominikh/go-mode.el
