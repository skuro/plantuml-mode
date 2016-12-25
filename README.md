[![MELPA](http://melpa.org/packages/plantuml-mode-badge.svg)](http://melpa.org/#/plantuml-mode) [![MELPA Stable](http://stable.melpa.org/packages/plantuml-mode-badge.svg)](http://stable.melpa.org/#/plantuml-mode) [![Build Status](https://travis-ci.org/skuro/plantuml-mode.svg?branch=master)](https://travis-ci.org/skuro/plantuml-mode)


# PlantUML mode for Emacs

<img src="https://raw.githubusercontent.com/skuro/plantuml-mode/master/banner.png"
 alt="plantuml-mode in action" title="Behold the beauty of text-based UML diagrams!" />

> “Pummel me all you want," I say. "Pummel me to death, in fact. My answers will not change."
> The Invierno step back, frowning. "You must love her very much," he says, not unkindly.”
>
> from [The Bitter Kingdom - Fire and Thorns #3](https://www.goodreads.com/book/show/11431896-the-bitter-kingdom)


A [PlantUML](http://plantuml.sourceforge.net/) major mode for Emacs.

# Installation

Make sure you have [MELPA](http://melpa.org/) enabled in your ``emacs.d``. Then, you can just

    M-x package-install<RET>
    plantuml-mode<RET>

Also, to enable preview you need to tell `plantuml-mode` where to locate the PlantUML JAR file. By default it will look for it in `~/plantuml.jar`, but you can specify any location with:

    M-x customize-variable<RET>
    plantuml-jar-path<RET>

# Features

- Syntax highlight
- Autocompletion
- Preview of buffer or region

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
  <hit C-' here to open a plantuml-mode buffer>
#+END_SRC
```

When in the `plantuml-mode` buffer you can then hit again `C-'` to return to the original `org-mode` document.

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

If you are using SVG rendering and `plantuml-preview` shows you an empty buffer, chances are something is wrong with the PlantUML output. While inside the preview buffer, hit `C-c C-c` to switch to the textual mode and see if the output is valid SVG.

# Credits

This project stemmed from the great work of [Zhang Weize](http://zhangweize.wordpress.com/2010/09/20/update-plantuml-mode/),
and the current code still borrows a lot from it.

Thanks to [Pavel G. Koukoushkin](https://github.com/svargellin) for implementing the preview functionality.

# See also

- [PlantUML](http://plantuml.com/)
- [`flycheck-plantuml`](https://github.com/alexmurray/flycheck-plantuml)

# License

Released under the terms of [GPLv2](http://www.gnu.org/licenses/gpl-2.0.html). See [LICENSE.txt](https://github.com/skuro/plantuml-mode/blob/master/LICENSE.txt).
