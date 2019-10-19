[![MELPA](http://melpa.org/packages/plantuml-mode-badge.svg)](http://melpa.org/#/plantuml-mode) [![MELPA Stable](http://stable.melpa.org/packages/plantuml-mode-badge.svg)](http://stable.melpa.org/#/plantuml-mode) [![CircleCI](https://circleci.com/gh/skuro/plantuml-mode.svg?style=svg)](https://circleci.com/gh/skuro/plantuml-mode) [![Coverage Status](https://coveralls.io/repos/github/skuro/plantuml-mode/badge.svg?branch=HEAD)](https://coveralls.io/github/skuro/plantuml-mode?branch=HEAD)

# PlantUML mode for Emacs

<img src="https://raw.githubusercontent.com/skuro/plantuml-mode/master/banner.png"
 alt="plantuml-mode in action" title="Behold the beauty of text-based UML diagrams!" />

> “Pummel me all you want," I say. "Pummel me to death, in fact. My answers will not change."
> The Invierno step back, frowning. "You must love her very much," he says, not unkindly.”
>
> from [The Bitter Kingdom - Fire and Thorns #3](https://www.goodreads.com/book/show/11431896-the-bitter-kingdom)

A [PlantUML](http://plantuml.sourceforge.net/) major mode for Emacs.

# Quick Guide

1. Install with `M-x package-install<RET>`
2. Enable mode for current buffer `M-x plantuml-mode<RET>`
3. Write some PlantUML:

```
@startuml
Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response

Alice -> Bob: Another authentication Request
Alice <-- Bob: Another authentication Response
@enduml
```

4. Preview diagrams with `C-c C-c` (`plantuml-preview`).

**WARNING:** This may send information to
`"https://www.plantuml.com/plantuml"`! Check that
`plantuml-default-exec-mode` is configured correctly for your use case
before previewing any sensitive material.

```lisp
    ;; Sample jar configuration
    (setq plantuml-jar-path "/path/to/your/copy/of/plantuml.jar")
    (setq plantuml-default-exec-mode 'jar)

    ;; Sample executable configuration
    (setq plantuml-executable-path "/path/to/your/copy/of/plantuml.bin")
    (setq plantuml-default-exec-mode 'executable)
```

See [Execution modes](#execution-modes) for more information.

# Installation

Make sure you have [MELPA](http://melpa.org/) enabled in your ``emacs.d``. Then, you can just

    M-x package-install<RET>
    plantuml-mode<RET>

Also, to enable preview you need to tell `plantuml-mode` where to locate the PlantUML JAR file. By default it will look for it in `~/plantuml.jar`, but you can specify any location with:

    M-x customize-variable<RET>
    plantuml-jar-path<RET>

You can also download the latest version of PlantUML straight into `plantuml-jar-path`:

    M-x plantuml-download-jar<RET>

# Features

- Syntax highlight
- Autocompletion
- Preview of buffer or region
- [EXPERIMENTAL] Use either local JAR or remote server for preview

# Enable the major mode

You can automatically enable `plantuml-mode` for files with extension `.plantuml` by adding the following to your `.emacsrc`:

    ;; Enable plantuml-mode for PlantUML files
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

Of course, you can always enable manually the major mode by typing `M-x plantuml-mode` once in the desired PlantUML file buffer.

# Usage

You can tell `plantuml-mode` to autocomplete the word before the cursor by typing `M-x plantuml-complete-symbol`. This will open a popup with all the available completions as found in the list of keywords given by running PlantUML with the `-language` flag.

To render the PlantUML diagram within Emacs, you can hit `M-x plantuml-preview`. This will run PlantUML and display the result in the `*PLANTUML-Preview*` buffer. The format used to render the diagram is automagically chosen from what's supported by your Emacs. It will be one of the following, in order of preference:

- SVG
- PNG
- Unicode ASCII art

The diagram will be either created from the selected region if one is available in the current buffer, or using the whole buffer otherwise.

If you want to force a specific output format, you can customize the variable `plantuml-output-type` to the value you prefer.

## Default key bindings

The following shortcuts are enabled by default:

    C-c C-c  plantuml-preview: renders a PlantUML diagram from the current buffer in the best supported format

    C-u C-c C-c  plantuml-preview in other window

    C-u C-u C-c C-c plantuml-preview in other frame

## Integration with `org-mode`

You can use `plantuml-mode` to edit PlantUML source snippets within an [`org-mode`](http://orgmode.org/) document. To do so, you need to first register it with the `plantuml` language:

```
(add-to-list
  'org-src-lang-modes '("plantuml" . plantuml))
```

Then you can edit a `plantuml` code block with `plantuml-mode` by hitting `C-'` while inside of the code block itself:

```elisp
#+BEGIN_SRC plantuml
  <hit C-c ' here to open a plantuml-mode buffer>
#+END_SRC
```

When in the `plantuml-mode` buffer you can then hit again `C-'` to return to the original `org-mode` document.

# Execution modes

**EXPERIMENTAL**

As of `v1.3.0` support is added for switching execution mode. The following two modes are available:

- `jar` (default) to run PlantUML as a local JAR file. This is the traditional system used by `plantuml-mode`
- `server` (experimental) to let an instance of [`plantuml-server`](https://github.com/plantuml/plantuml-server) render the preview
- `executable` to run PlantUML as a local executable file. This is useful if your package manager provides a executable for PlantUML.

You can customize `plantuml-default-exec-mode` or run `plantuml-set-exec-mode` from a `plantuml-mode` buffer to switch modes.

## Configure server rendering

When selecting `server` execution modes, you can customize the following variable to set the server to use for rendering:

```
plantuml-server-url
```

It defaults to `"https://www.plantuml.com/plantuml"`.

# Migration from `puml-mode`

If you were previously using `puml-mode`, you should change any reference to a `puml-..` variable or function to its `plantuml-..` counterpart. Most notably, `puml-plantuml-jar-path` is now just `plantuml-jar-path`.

## Deprecation warning

If `puml-mode` is still being loaded by your Emacs, a Warning will be shown any time you open a PlantUML file. This is to remind you that you are running deprecated software. If you want to suppress the (intentionally) annoying warning, you can customize the variable `plantuml-suppress-deprecation-warning`.

# Troubleshooting

## Debug messages

As of `v0.6.2` the following commands have been added to help resolving problems:

```
plantuml-enable-debug
plantuml-disable-debug
```

With debug enabled, the actual command used to invoke PlantUML is printed into a buffer called `*PLANTUML Messages*`. You can inspect that buffer to make sure that PlantUML is properly set up, or use it to supply extra information when opening [issues](https://github.com/skuro/plantuml-mode/issues).

## Blank previews

If you are using SVG rendering and `plantuml-preview` shows you an empty buffer, chances are something is wrong with the PlantUML output. While inside the preview buffer, hit `C-c C-c` to switch to
the textual mode and see if the output is valid SVG.

## Development

Development happens mostly on the `develop` branch, which is released on MELPA at every commit. The `master` branch is used to generate tags which are then released to [MELPA
stable](https://stable.melpa.org).

In order to contribute to `plantuml-mode` make sure to:

- agree with the [GPLv3+ licencing terms](#License) as they are mandated by Emacs
- install [`cask`](https://github.com/cask/cask)
- always test your code and run the full test suite with `cask exec ert-runner` (or just by `make`)
- ensure you always target `develop` in your pull requests

For collaborators who have merge access to the repo:
- make sure [`set-version.sh`](./blob/develop/bin/set-version.sh) is run as part of your [`pre-commit` git hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)
- always merge PRs on the command line
- when releasing a new stable version, add the proper entry in the changelog within `plantuml-mode.el` to ensure the right version is set for `plantuml-mode-version`

# Credits

This project stemmed from the great work of [Zhang Weize](http://zhangweize.wordpress.com/2010/09/20/update-plantuml-mode/),
and the current code still borrows a lot from it.

Thanks to [Pavel G. Koukoushkin](https://github.com/svargellin) for implementing the preview functionality.
Thanks to [Raymond Huang](https://github.com/rymndhng) for implementing the first drop of the indentation support.

Thanks to [all the awesome contributors](https://github.com/skuro/plantuml-mode/graphs/contributors), you rock!

# See also

- [PlantUML](http://plantuml.com/)
- [`flycheck-plantuml`](https://github.com/alexmurray/flycheck-plantuml)

# License

Released under the terms of [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) or (at your option) any later version. See [LICENSE.txt](https://github.com/skuro/plantuml-mode/blob/master/LICENSE.txt).
