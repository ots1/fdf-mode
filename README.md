## Synopsis

Fdf-mode provides emacs major modes for editing files in the
Flexible Data Format (FDF), which is the input file format used by the
[Siesta](http://departments.icmab.es/leem/siesta/) electronic
structure code.  Font-locking (syntax highlighting),
indentation and autocompletion are provided.

FDF is a generic configuration file format.  Siesta supports specific
keywords that can appear in the configuration file.  See section 4 of
the [Siesta manual](http://departments.icmab.es/leem/siesta/Documentation/Manuals/manuals.html).

This project provides two major modes: a mode for editing generic FDF files
(`fdf-mode`), and a mode for Siesta-specific FDF files derived from it
(`siesta-mode`).

## Installation

The files with extension `.el` should be visible to emacs via the load
path.  If these files are placed in `~/.emacs.d/site-lisp/fdf-mode/`,
then this is accomplished by adding the following lines to your
`~/.emacs` file:

```
(add-to-list 'load-path "~/.emacs.d/site-lisp/fdf-mode/")
(autoload 'fdf-mode "fdf-mode" "" t)
(autoload 'siesta-mode "siesta-mode" "" t)
```

To select `siesta-mode` automatically when opening files with an
`.fdf` extension, add the following to your `~/.emacs`:

```
(add-to-list 'auto-mode-alist '("\\.fdf\\'" . siesta-mode))
```

## Usage 

In addition to font-locking and indentation, autocompletion is
provided via the
[hippie-expand](https://www.gnu.org/software/emacs/manual/html_node/autotype/Hippie-Expand.html)
functionality of emacs.  Running `M-x hippie-expand` on a partially
typed keyword will attempt to complete it as an FDF (or Siesta)
keyword, and will thereafter cycle through all possible completions.

A multiline `block` in FDF looks like

```
%block label 
statement1 
statement2
%endblock label 
``` 

A block may be closed automatically (inserting `%endblock label`)
by running `fdf-close-block`, by default bound to the keyboard
sequence `C-c C-e`.
