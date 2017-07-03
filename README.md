# rufo-mode.el

TODO: add to melpa

Minor-mode to automatically format ruby with [rufo][].

## Usage

Ensure that you have [rufo][] installed and in your path.

Then, in your `init.el`:

```elisp
(add-hook 'ruby-mode 'rufo-mode)
```

## Options

`rufo-mode` has first-class support for using rufo with bundler

```elisp
(setq rufo-mode-use-bundler t)
```

You can change the location of [rufo][]. 

```elisp
(setq rufo-mode-executable "rufa")
```

You can enable `rufo-mode-debug-mode` which will output additional info into the messages buffer

## Thanks

* [@aaronjensen][] for [eslintd-fix][], which I forked for this project

[rufo]: https://github.com/asterite/rufo
[eslintd-fix]: https://github.com/aaronjensen/eslintd-fix
[@aaronjensen]: https://github.com/aaronjensen
