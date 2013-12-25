req-package
===========

##### Description

req-package is a macro wrapper on top of use-package.
It's goal is to simplify package dependencies management
when using use-package for your .emacs.

##### Usage

* load req-package:

```elisp
(require 'req-package)
```

* define required packages with dependencies using **:require** like this:

```elisp
   (req-package dired
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
* to start loading packages in right order:

```elisp
   (req-package-finish)
```

##### Migrate from use-package

Just replace all `(use-package ...)` with `(req-package [:require DEPS] ...)` and add `(req-package-finish)` at the end of your configuration file.

##### Note

All use-package parameters are supported, see use-package manual
for additional info.

Also there are possible troubles with deferred loading provided by use-package.
If you want to use it, try defer all packages in one dependency tree.
