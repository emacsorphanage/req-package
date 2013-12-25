req-package
===========

##### Description

Req-package is a macro wrapper on top of use-package.
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

##### Note

All use-package parameters are supported, see use-package manual
for additional info.

Also there are possible troubles with deferred loading provided by use-package.
If you want to use it, try defer all packages in one dependency tree.
