# rufo.el

TODO: add to melpa

Minor-mode to automatically format ruby with [rufo][].

## Usage

Ensure that you have [rufo][] installed and in your path.

Then, in your `init.el`:

```elisp
(add-hook 'ruby-mode 'rufo-mode)
```

## Options

You can change the location of [rufo][]. 

```elisp
(setq rufo-mode-executable "bundle exec rufo")
```

## Thanks

* [@aaronjensen][] for [eslintd-fix][], which I forked for this project

[rufo]: https://github.com/asterite/rufo
[eslint_d-fix]: https://github.com/aaronjensen/eslintd-fix
[@aaronjensen]: https://github.com/aaronjensen
