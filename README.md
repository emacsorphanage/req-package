req-package
===========

##### Description

req-package is a macro wrapper on top of use-package.
It's goal is to simplify package dependencies management,
when using use-package for your .emacs.

##### Usage

* Load req-package:

```elisp
(require 'req-package)
```

* Define required packages with dependencies using `:require` like this:

```elisp
   (req-package dired) ;; you can omit this empty requirement because of dired-single

   (req-package dired-single
     :require dired
     :init (...))

   (req-package lua-mode
     :init (...))

   (req-package flymake)

   (req-package flymake-lua
     :require (flymake lua-mode)
     :init (...))
```
* To start loading packages in right order:

```elisp
   (req-package-finish)
```

After calling **req-package-finish** targets list is cleaned and
you can start requiring and loading a new bunch of packages.
It may be useful when you need to load some package after all.

**Note** req-package expects `:require` keyword strongly after package name.

Do not write something like this:

```elisp
    (req-package foo :ensure bar :require baz) ;; WRONG
```

It won't work. The right code will be:

```elisp
    (req-package foo :require baz :ensure bar) ;; RIGHT
```

##### Migrate from use-package

Just replace all `(use-package ...)` with `(req-package [:require DEPS] ...)` and add `(req-package-finish)` at the end of your configuration file.

##### Note

All use-package parameters are supported, see use-package manual
for additional info.

Also there are possible troubles with `:defer` keyword provided by use-package.
If you want to use it, try `:defer` all packages in one dependency tree.

More complex req-package usage example can be found at http://github.com/edvorg/emacs-configs.

##### Contribute

Please, commit and pull-request your changes to **develop** branch.
Master is used for automatic repo package builds by melpa's travis-ci.

##### Changelog

* **v0.4.2**:
    Bug fixes.
* **v0.4.1**:
    Various tweaks and bug fixes.
* **v0.4-all-cycles**:
    All cycles of your dependencies will be printed now.
    Also there are more handy log messages and some bug fixes.
* **v0.3-cycles**:
    There are nice error messages about cycled dependencies now.
    Cycles printed in a way: `pkg1 -> [pkg2 -> ...] pkg1`.
    It means there is a cycle around `pkg1`.
* **v0.2-auto-fetch**:
    There is no need of explicit `:ensure` in your code now.
    When you req-package it adds `:ensure` if package is available in your repos.
    Also package deps `:ensure`'d automatically too.
    Just write `(req-package pkg1 :require pkg2)` and all you need will be installed.
