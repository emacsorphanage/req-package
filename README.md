req-package
===========

req-package is a macro wrapper on top of use-package
it's goal is to simplify package dependencies management
when using use-package for your .emacs

usage:

1) (require 'req-package)
2) define required packages with dependencies using :require like this:
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
3) (req-package-finish) to start loading packages in right order

note:

all use-package parameters are supported, see use-package manuals
for additional info.

there are possible troubles with deferred loading provided by use-package.
if you want to use it, try defer all packages in one dependency tree.
