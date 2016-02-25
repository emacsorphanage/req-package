;;; req-package.el --- A use-package wrapper for package runtime dependencies management

;; Copyright (C) 2013-2014 Edward Knyshov

;; Author: Edward Knyshov <edvorg@gmail.com>
;; Created: 25 Dec 2013
;; Version: 0.9
;; Package-Requires: ((use-package "1.0") (dash "2.7.0") (log4e "0.2.0") (ht "0"))
;; Keywords: dotemacs startup speed config package
;; X-URL: https://github.com/edvorg/req-package

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Table of Contents
;; ─────────────────

;; 1 req-package
;; .. 1.1 Description
;; .. 1.2 Usage
;; .. 1.3 El Get
;; .. 1.4 More?
;; .. 1.5 Migrate from use-package
;; .. 1.6 Note
;; .. 1.7 Logging
;; .. 1.8 Contribute
;; .. 1.9 Things to be done
;; ..... 1.9.1 TODO take package dependencies from it's meta data
;; ..... 1.9.2 TODO el-get/elpa packages must be in priority over builtin ones
;; .. 1.10 Changelog
;; ..... 1.10.1 `v1.0'
;; ..... 1.10.2 `v0.9'
;; ..... 1.10.3 `v0.8'
;; ..... 1.10.4 `v0.7'
;; ..... 1.10.5 `v0.6'
;; ..... 1.10.6 `v0.5'
;; ..... 1.10.7 `v0.4.2'
;; ..... 1.10.8 `v0.4.1'
;; ..... 1.10.9 `v0.4-all-cycles'
;; ..... 1.10.10 `v0.3-cycles'
;; ..... 1.10.11 `v0.2-auto-fetch'


;; 1 req-package
;; ═════════════

;;   [[file:https://img.shields.io/badge/license-GPL_3-green.svg]]
;;   [[file:http://melpa.org/packages/req-package-badge.svg]]
;;   [[file:http://stable.melpa.org/packages/req-package-badge.svg]]
;;   [[file:https://travis-ci.org/edvorg/req-package.svg]]
;;   [[file:https://coveralls.io/repos/edvorg/req-package/badge.svg?branch=develop&service=github]]


;;   [[file:https://img.shields.io/badge/license-GPL_3-green.svg]]
;;   http://www.gnu.org/licenses/gpl-3.0.txt

;;   [[file:http://melpa.org/packages/req-package-badge.svg]]
;;   http://melpa.org/#/req-package

;;   [[file:http://stable.melpa.org/packages/req-package-badge.svg]]
;;   http://stable.melpa.org/#/req-package

;;   [[file:https://travis-ci.org/edvorg/req-package.svg]]
;;   https://travis-ci.org/edvorg/req-package

;;   [[file:https://coveralls.io/repos/edvorg/req-package/badge.svg?branch=develop&service=github]]
;;   https://coveralls.io/github/edvorg/req-package?branch=develop


;; 1.1 Description
;; ───────────────

;;   req-package solves one single problem - make order of package
;;   configurations in your init.el right without continuous reordering
;;   your code while still providing ambrosian [use-package] goodness.  It
;;   makes your .emacs.d code more strict and modular, and less error
;;   prone.  You can look here, how I divided my code in separate modules
;;   and how simple it looks
;;   [https://github.com/edvorg/emacs-configs/tree/master/init.d] .

;;   Remember, how often you tackled into problem, when you need to require
;;   one package, do some configuration, then the same with second and so
;;   on. Sometimes it becomes too complex.  Especially in cases when one
;;   package have more than one dependency.  You can draw a graph of
;;   dependencies in your configuration, and, I'm sure, it's complex.
;;   req-package creates this graph for you and makes a correct traverse on
;;   it.  The syntax is almost the same as with use-package, but it
;;   provides a few additional keywords:
;;   1) :require - a parameter to specify dependencies
;;   2) :loader - an optional parameter to specify where to get package
;;      (el-get, elpa, etc.)

;;   Interesting thing is that packages are installed automatically once
;;   req-package-finish function is executed.  So there is no need for
;;   things like cask or save-packages.  You just write a configuration
;;   with packages you need and they will be there.  req-package will try
;;   to use elpa, el-get or any package system provided by you to find and
;;   install your packages.


;;   [use-package] https://github.com/jwiegley/use-package


;; 1.2 Usage
;; ─────────

;;   Load req-package:

;;   ┌────
;;   │ (require 'req-package)
;;   └────

;;   Define required packages with dependencies using `:require' like this:

;;   ┌────
;;   │ (req-package dired) ;; you can omit this empty requirement because of dired-single
;;   │
;;   │ (req-package dired-single
;;   │   :require dired
;;   │   :config (...))
;;   │
;;   │ (req-package lua-mode :loader :elpa
;;   │   :config (...))
;;   │
;;   │ (req-package flymake :loader :el-get)
;;   │
;;   │ (req-package flymake-lua
;;   │   :require flymake lua-mode
;;   │   :config (...))
;;   └────

;;   To start loading packages in right order:

;;   ┌────
;;   │ (req-package-finish)
;;   └────


;; 1.3 El Get
;; ──────────

;;   There is another benefit over use-package - `el-get' support.  No more
;;   thinking about sources for your packages.  Just install and configure
;;   your el-get.  Here is example:

;;   ┌────
;;   │ (require 'req-package)
;;   │
;;   │ (req-package-force el-get
;;   │   :init
;;   │   (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;;   │   (el-get 'sync))
;;   │
;;   │ (req-package gotham-theme
;;   │   :config
;;   │   (print "gotham theme is here and installed from el-get"))
;;   │
;;   │ (req-package-finish)
;;   └────

;;   Also, of course, there could be dependencies between el-get and elpa
;;   packages


;; 1.4 More?
;; ─────────

;;   You can always extend list of package providers or change priorities
;;   if you want.  in which your packages are being installed.  It can be
;;   done by customizing `req-package-providers' map.  It's a mapping
;;   loader-symbol -> (list install-function package-present-p-function)


;; 1.5 Migrate from use-package
;; ────────────────────────────

;;   Just replace all `(use-package ...)' with `(req-package [:require
;;   DEPS] ...)' and add `(req-package-finish)' at the end of your
;;   configuration file.


;; 1.6 Note
;; ────────

;;   All use-package parameters are supported, see use-package manual.  for
;;   additional info.

;;   However, there is no need for the `:ensure' keyword; req-package will
;;   add it automatically if needed.

;;   For each package you can manually specify loader function by `:loader'
;;   keyword.  It can be any key for `req-package-providers' map.

;;   Also there is a `req-package-force' function which simulates plain old
;;   use-package behavior.

;;   More complex req-package usage example can be found at
;;   [http://github.com/edvorg/emacs-configs].


;; 1.7 Logging
;; ───────────

;;   You can use `req-package--log-open-log' to see, what is happening with
;;   your configuration.  You can choose log level in `req-package' group
;;   by `req-package-log-level' custom.  These log levels are supported:
;;   `fatal', `error', `warn', `info', `debug', `trace'.


;; 1.8 Contribute
;; ──────────────

;;   Please, commit and pull-request your changes to `develop' branch.
;;   Master is used for automatic repo package builds by melpa's travis-ci.


;; 1.9 Things to be done
;; ─────────────────────

;; 1.9.1 TODO take package dependencies from it's meta data
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌


;; 1.9.2 TODO el-get/elpa packages must be in priority over builtin ones
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌


;; 1.10 Changelog
;; ──────────────

;; 1.10.1 `v1.0'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • once you called `req-package-finish' you are able reload package
;;     just by reload `req-package' form
;;   • proper errors handling. see `req-package--log-open-log' for messages
;;   • smart add-hook which invokes function if mode is loaded
;;   • refactor providers system
;;   • no need to use progn in :init and :config sections
;;   • no need to use list literal in :require section
;;   • `:loader' keyword now accepts loaders as keywords or as
;;     functions. e.g. `:el-get', `:elpa', `my-loader-fn'


;; 1.10.2 `v0.9'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • `:loader' keyword support


;; 1.10.3 `v0.8'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • bugfixes


;; 1.10.4 `v0.7'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • fixed some issues with packages installation. all packages will be
;;     installed at bootstrap time
;;   • custom package providers support by `req-package-providers'
;;   • priority feature for cross provider packages loading. you can
;;     choose, what to try first - elpa, el-get, or something else


;; 1.10.5 `v0.6'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • `el-get' support


;; 1.10.6 `v0.5'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • Major system refactoring.
;;   • Fixed bugs with defered loading.
;;   • Significant performance optimization.
;;   • `max-specpdl-size', `max-lisp-eval-depth' issues completely solved.
;;   • Flexible `:require' keyword parsing.


;; 1.10.7 `v0.4.2'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • Bug fixes.


;; 1.10.8 `v0.4.1'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • Various tweaks and bug fixes.


;; 1.10.9 `v0.4-all-cycles'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • All cycles of your dependencies will be printed now.
;;   • Also there are more handy log messages and some bug fixes.


;; 1.10.10 `v0.3-cycles'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • There are nice error messages about cycled dependencies now.
;;   • Cycles printed in a way: `pkg1 -> [pkg2 -> ...] pkg1'.
;;   • It means there is a cycle around `pkg1'.


;; 1.10.11 `v0.2-auto-fetch'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • There is no need of explicit `:ensure' in your code now.
;;   • When you req-package it adds `:ensure' if package is available in
;;     your repos.
;;   • Also package deps `:ensure''d automatically too.
;;   • Just write `(req-package pkg1 :require pkg2)' and all you need will
;;     be installed.

;;; Code:

(eval-when-compile (require 'cl))
(require 'package)

(defun req-package-bootstrap (package)
  "Refresh package archives, check PACKAGE presence and install if it's not installed."
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package))))))

(req-package-bootstrap 'use-package)
(req-package-bootstrap 'dash)
(req-package-bootstrap 'log4e)
(req-package-bootstrap 'ht)

(require 'use-package)
(require 'dash)
(require 'log4e)
(require 'ht)

(require 'req-package-providers)
(require 'req-package-hooks)
(require 'req-package-args)
(require 'req-package-cycles)

(defgroup req-package nil
  "A package loading system"
  :group 'emacs)

(defcustom req-package-log-level 'info
  "Minimal log level, may be any level supported by log4e."
  :group 'req-package)

(defvar req-package-required-by (make-hash-table :size 200 :test 'equal)
  "Package symbol -> list of packages dependent on it.")

(defvar req-package-deps-left (make-hash-table :size 200 :test 'equal)
  "Package symbol -> list of packages dependent on it.")

(defvar req-package-evals (make-hash-table :size 200 :test 'equal)
  "Package symbol -> loading code prepared for evaluation.")

(defvar req-package-loaders (make-hash-table :size 200 :test 'equal)
  "Package symbol -> loader function to load package by.")

(defun req-package-patch-config (name form)
  "Wrap package NAME :config FORM into progn with callbacks."
  (list 'progn
        (list 'req-package-handle-loading (list 'quote name) (list 'lambda () form))
        (list 'req-package-loaded (list 'quote name))))

(defun req-package-eval (name)
  "Evaluate package NAME request."
  (let* ((DEFAULT (req-package-gen-eval name (list 'progn) (req-package-patch-config name nil) nil))
         (EVAL (gethash name req-package-evals DEFAULT))
         (NAME name))
    (req-package-handle-loading NAME (lambda () (eval EVAL)))))

(defun req-package-loaded (name)
  "Called after package NAME loaded to continue dependency graph traverse."
  (req-package--log-info "package loaded: %s" name)
  (let* ((EVALS (-reduce-from
                 (lambda (memo dependent)
                   (let* ((DEPS-LEFT (- (gethash dependent req-package-deps-left 0) 1)))
                     (puthash dependent DEPS-LEFT req-package-deps-left)
                     (if (equal 0 DEPS-LEFT) (cons dependent memo) memo)))
                 nil
                 (gethash name req-package-required-by nil))))
    (-each EVALS (lambda (name)
                   (puthash name -1 req-package-deps-left)
                   (req-package-eval name)))))

(defun req-package-handle-loading (name f)
  "Error handle for package NAME loading process by calling F."
  (condition-case-unless-debug e
      (funcall f)
    (error (req-package--log-error (format "Unable to load package %s -- %s" name e)))))

(defun req-package-gen-eval (package init config rest)
  "Generate eval for PACKAGE."
  (append (list 'use-package package)
          (list :init init)
          (list :config config)
          rest))

(defmacro req-package (name &rest args)
  "Add package NAME with ARGS to target list."
  `(let* ((NAME ',name)
          (ARGS ',args)
          (SPLIT1 (req-package-args-extract-arg :require ARGS nil))
          (SPLIT2 (req-package-args-extract-arg :loader (car (cdr SPLIT1)) nil))
          (SPLIT3 (req-package-args-extract-arg :init (car (cdr SPLIT2)) nil))
          (SPLIT4 (req-package-args-extract-arg :config (car (cdr SPLIT3)) nil))
          (DEPS (-flatten (car SPLIT1)))
          (LOADER (caar SPLIT2))
          (INIT (cons 'progn (car SPLIT3)))
          (CONFIG (req-package-patch-config NAME (cons 'progn (car SPLIT4))))
          (REST (cadr SPLIT4))
          (EVAL (req-package-gen-eval NAME INIT CONFIG REST))
          (DEPS-LEFT (gethash NAME req-package-deps-left 0)))
     (req-package--log-debug "package requested: %s" NAME)
     (puthash NAME LOADER req-package-loaders)
     (puthash NAME EVAL req-package-evals)
     (puthash NAME (gethash NAME req-package-deps-left 0) req-package-deps-left)
     (if (= DEPS-LEFT -1)
         (progn (eval EVAL)
                DEPS-LEFT)
       (progn
         (puthash NAME 0 req-package-deps-left)
         (-each DEPS
           (lambda (req)
             (let* ((REQUIRED-BY (gethash req req-package-required-by nil))
                    (DEPS-LEFT (gethash NAME req-package-deps-left 0))
                    (REQ-DEPS-LEFT (gethash req req-package-deps-left 0)))
               (puthash req (gethash req req-package-deps-left 0) req-package-deps-left)
               (when (not (equal -1 REQ-DEPS-LEFT))
                 (puthash req (cons NAME REQUIRED-BY) req-package-required-by)
                 (puthash NAME (+ DEPS-LEFT 1) req-package-deps-left)))))))))

(defmacro req-package-force (name &rest args)
  "Immediatly load package NAME with ARGS."
  `(let* ((NAME ',name)
          (ARGS ',args)
          (SPLIT1 (req-package-args-extract-arg :require ARGS nil))
          (SPLIT2 (req-package-args-extract-arg :loader (car (cdr SPLIT1)) nil))
          (SPLIT3 (req-package-args-extract-arg :init (car (cdr SPLIT2)) nil))
          (SPLIT4 (req-package-args-extract-arg :config (car (cdr SPLIT3)) nil))
          (DEPS (-flatten (car SPLIT1)))
          (LOADER (caar SPLIT2))
          (INIT (cons 'progn (car SPLIT3)))
          (CONFIG (req-package-patch-config NAME (cons 'progn (car SPLIT4))))
          (REST (cadr SPLIT4))
          (EVAL (req-package-gen-eval NAME INIT CONFIG REST)))
     (req-package--log-debug "package force-requested: %s" NAME)
     (req-package-handle-loading NAME
                      (lambda ()
                        (req-package-providers-prepare NAME LOADER)
                        (eval EVAL)))))

(defun req-package-finish ()
  "Start loading process, call this after all req-package invocations."
  (req-package-cycles-detect req-package-required-by)
  (req-package--log-debug "package requests finished: %s packages are waiting"
               (hash-table-count req-package-deps-left))
  (maphash (lambda (key value)
             (req-package-providers-prepare key (gethash key req-package-loaders nil)))
           req-package-deps-left)
  (maphash (lambda (key value)
             (if (equal (gethash key req-package-deps-left 0) 0)
                 (progn (puthash key -1 req-package-deps-left)
                        (req-package-eval key))))
           req-package-deps-left))

(put 'req-package 'lisp-indent-function 'defun)
(put 'req-package-force 'lisp-indent-function 'defun)
(put 'req-package-hooks-add-execute 'lisp-indent-function 'defun)
(put 'req-package-hooks-add-execute-impl 'lisp-indent-function 'defun)

(defconst req-package-font-lock-keywords
  '(("(\\(req-package\\|req-package-force\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode req-package-font-lock-keywords)

(log4e:deflogger "req-package" "%t [%l] %m" "%H:%M:%S")
(req-package--log-set-level req-package-log-level)
(req-package--log-enable-logging)
(req-package--log-clear-log)

(provide 'req-package)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; req-package.el ends here
