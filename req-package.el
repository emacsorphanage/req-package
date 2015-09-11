;;; req-package.el --- A use-package wrapper for package runtime dependencies management

;; Copyright (C) 2013-2014 Edward Knyshov

;; Author: Edward Knyshov <edvorg@gmail.com>
;; Created: 25 Dec 2013
;; Version: 0.9
;; Package-Requires: ((use-package "1.0") (dash "2.7.0") (log4e "0.2.0"))
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
;; ..... 1.10.1 v1.0
;; ..... 1.10.2 v0.9
;; ..... 1.10.3 v0.8
;; ..... 1.10.4 v0.7
;; ..... 1.10.5 v0.6
;; ..... 1.10.6 v0.5
;; ..... 1.10.7 v0.4.2
;; ..... 1.10.8 v0.4.1
;; ..... 1.10.9 v0.4-all-cycles
;; ..... 1.10.10 v0.3-cycles
;; ..... 1.10.11 v0.2-auto-fetch


;; 1 req-package
;; ═════════════

;; 1.1 Description
;; ───────────────

;;   req-package solves one single problem - make order of package
;;   configurations in your init.el right without continuous reordering
;;   your code while still providing ambrosian [use-package] goodness.  It
;;   makes your .emacs.d code more strict, modular and error prone.  You
;;   can look here, how I divided my code in separate modules and how
;;   simple it looks
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
;;   │ (req-package lua-mode
;;   │   :config (...))
;;   │
;;   │ (req-package flymake)
;;   │
;;   │ (req-package flymake-lua
;;   │   :require (flymake lua-mode)
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
;;   │ (require 'req-package'')
;;   │
;;   │ (req-package-force el-get
;;   │   :init (progn (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;;   │                (el-get 'sync)))
;;   │
;;   │ (req-package gotham-theme
;;   │   :config (print "gotham theme is here and installed from el-get"))
;;   │
;;   │ (req-package-finish)
;;   └────

;;   Also, of course, there could be dependencies between el-get and elpa
;;   packages


;; 1.4 More?
;; ─────────

;;   You can always extend list of package providers or change priorities
;;   if you want.  in which your packages are being installed.  It can be
;;   done by customizing `req-package-providers' list.  It's list of
;;   functions, which can install packages.

;;   Here are some rules for one such function:

;;   • check package presence at corresponding repo
;;   • check whether it installed or not
;;   • install that package if it is available and not installed
;;   • return nonnil only if package is installed already or successfully
;;     installed by this function"


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

;;   For each package you can manually specify loader fuction by `:loader'
;;   keyword.  It can be any acceptable item for `req-package-providers'
;;   list.

;;   Also there is a `req-package-force' function which simulates plain old
;;   use-package behavior.

;;   More complex req-package usage example can be found at
;;   [http://github.com/edvorg/emacs-configs].


;; 1.7 Logging
;; ───────────

;;   You cand use `req-package--log-open-log' to see, what is happening
;;   with your configuration.  You can choose log level in `req-package'
;;   group by `req-package-log-level' custom.  These log levels are
;;   supported: `fatal', `error', `warn', `info', `debug', `trace'.


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

;; 1.10.1 v1.0
;; ╌╌╌╌╌╌╌╌╌╌╌

;;   • proper errors handling. see `req-package–log-open-log` for messages


;; 1.10.2 v0.9
;; ╌╌╌╌╌╌╌╌╌╌╌

;;   • `:loader' keyword support


;; 1.10.3 v0.8
;; ╌╌╌╌╌╌╌╌╌╌╌

;;   • bugfixes


;; 1.10.4 v0.7
;; ╌╌╌╌╌╌╌╌╌╌╌

;;   • fixed some issues with packages installation. all packages will be
;;     installed at bootstrap time
;;   • custom package providers support by `req-package-providers'
;;   • priority feature for cross provider packages loading. you can
;;     choose, what to try first - elpa, el-get, or something else


;; 1.10.5 v0.6
;; ╌╌╌╌╌╌╌╌╌╌╌

;;   • `el-get' support


;; 1.10.6 v0.5
;; ╌╌╌╌╌╌╌╌╌╌╌

;;   • Major system refactoring.
;;   • Fixed bugs with defered loading.
;;   • Significant performance optimization.
;;   • `max-specpdl-size', `max-lisp-eval-depth' issues completely solved.
;;   • Flexible `:require' keyword parsing.


;; 1.10.7 v0.4.2
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • Bug fixes.


;; 1.10.8 v0.4.1
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • Various tweaks and bug fixes.


;; 1.10.9 v0.4-all-cycles
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • All cycles of your dependencies will be printed now.
;;   • Also there are more handy log messages and some bug fixes.


;; 1.10.10 v0.3-cycles
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • There are nice error messages about cycled dependencies now.
;;   • Cycles printed in a way: `pkg1 -> [pkg2 -> ...] pkg1'.
;;   • It means there is a cycle around `pkg1'.


;; 1.10.11 v0.2-auto-fetch
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • There is no need of explicit `:ensure' in your code now.
;;   • When you req-package it adds `:ensure' if package is available in
;;     your repos.
;;   • Also package deps `:ensure''d automatically too.
;;   • Just write `(req-package pkg1 :require pkg2)' and all you need will
;;     be installed.

;;; Code:

(eval-when-compile (require 'cl))
(require 'use-package)
(require 'package)
(require 'dash)
(require 'log4e)

(defgroup req-package nil
  "A package loading system"
  :group 'emacs)

(defcustom req-package-log-level 'warn
  "minimal log level. may be any level supported by log4e"
  :group 'req-package)

(defcustom req-package-providers '(req-package-try-elpa req-package-try-el-get)
  "list of functions to prepare packages installation
one such function should
1) check package presence at corresponding repo
2) check whether it installed or not
3) install that package if it available and not installed
4) return nonnil only if package is installed already or
   successfully installed by this function"
  :group 'req-package
  :type 'list)

(defvar req-package-reqs-reversed (make-hash-table :size 200)
  "package symbol -> list of packages dependent on it")

(defvar req-package-ranks (make-hash-table :size 200)
  "package symbol -> list of packages dependent on it")

(defvar req-package-evals (make-hash-table :size 200)
  "package symbol -> loading code prepared for evaluation")

(defvar req-package-loaders (make-hash-table :size 200)
  "package symbol -> loader function to load package by")

(defvar req-package-cycles-count 0
  "number of cycles detected")

(defconst req-package-el-get-present (if (require 'el-get nil t) t nil)
  "you can check this for el get presense")

(defun req-package-add-hook-execute-impl (m h f)
  "add function F to hook H and execute it if mode M is already activated"
  (if (or (assoc m minor-mode-list) (equal major-mode m))
      (funcall f))
  (add-hook h f))

(defun req-package-add-hook-execute (m f)
  "add function F to mode M and execute it if already activated"
  (let ((h (intern (concat (symbol-name m) "-hook"))))
    (req-package-add-hook-execute-impl m h f)))

(defun req-package-wrap-args (reqs)
  "listify passed dependencies"
  (if (atom reqs) (list reqs) reqs))

(defun req-package-extract-arg (key args acc)
  "extract dependencies from arg list"
  (if (null args)
      (list nil (reverse acc))
    (if (eq (car args) key)
        (list (req-package-wrap-args (car (cdr args)))
              (append (reverse acc) (cddr args)))
      (req-package-extract-arg key (cdr args) (cons (car args) acc)))))

(defun req-package-patch-config (name args)
  "patch :config section to invoke our callback"
  (let ((callback (list 'req-package-loaded (list 'quote name))))
    (if (null args)
        (list ':config callback)
      (if (eq (car args) :config)
          (cons ':config
                (cons (list 'progn
                            (list 'req-package-handle-loading (list 'quote name) (list 'lambda () (car (cdr args))))
                            callback)
                      (cddr args)))
        (cons (car args) (req-package-patch-config name (cdr args)))))))

(defun req-package-eval (name)
  "evaluate package request"
  (let* ((EVAL (gethash name
                        req-package-evals
                        (append (req-package-gen-eval name)
                                (req-package-patch-config name
                                                          nil))))
         (NAME name))
    (req-package-handle-loading NAME (lambda () (eval EVAL)))))

(defun req-package-loaded (name)
  "callback for dependency graph load continuation"
  (req-package--log-info "package loaded: %s" name)
  (let* ((EVALS (-reduce-from
                 (lambda (memo dependent)
                   (let* ((RANK (- (gethash dependent req-package-ranks 0) 1)))
                     (puthash dependent RANK req-package-ranks)
                     (if (eq 0 RANK) (cons dependent memo) memo)))
                 nil
                 (gethash name req-package-reqs-reversed nil))))
    (-each EVALS (lambda (name)
                   (puthash name -1 req-package-ranks)
                   (req-package-eval name)))))

(defmacro req-package (name &rest args)
  "add package to target list"
  `(let* ((NAME ',name)
          (ARGS ',args)
          (SPLIT1 (req-package-extract-arg :require ARGS nil))
          (SPLIT2 (req-package-extract-arg :loader (car (cdr SPLIT1)) nil))
          (USEPACKARGS (req-package-patch-config NAME (car (cdr SPLIT2))))
          (REQS (car SPLIT1))
          (LOADER (car SPLIT2)))

     (req-package--log-debug "package requested: %s" NAME)

     (-each REQS
       (lambda (req)
         (let* ((CURREQREV (gethash req req-package-reqs-reversed nil))
                (CURRANK (gethash NAME req-package-ranks 0)))
           (puthash req (cons NAME CURREQREV) req-package-reqs-reversed)
           (puthash req (gethash req req-package-ranks 0) req-package-ranks)
           (puthash NAME (+ CURRANK 1) req-package-ranks))))
     (puthash NAME LOADER req-package-loaders)
     (puthash NAME (append (req-package-gen-eval NAME) USEPACKARGS) req-package-evals)
     (puthash NAME (gethash NAME req-package-ranks 0) req-package-ranks)
     (puthash NAME (gethash NAME req-package-reqs-reversed nil) req-package-reqs-reversed)))

(defmacro req-package-force (name &rest args)
  "immediatly load some package"
  `(let* ((NAME ',name)
          (ARGS ',args)
          (SPLIT1 (req-package-extract-arg :require ARGS nil))
          (SPLIT2 (req-package-extract-arg :loader (car (cdr SPLIT1)) nil))
          (USEPACKARGS (req-package-patch-config NAME (car (cdr SPLIT2))))
          (REQS (car SPLIT1))
          (LOADER (car SPLIT2))
          (EVAL (append (req-package-gen-eval NAME) USEPACKARGS)))
     (req-package--log-debug "package force-requested: %s" NAME)
     (req-package-handle-loading NAME
                                 (lambda ()
                                   (req-package-prepare NAME LOADER)
                                   (eval EVAL)))))

(defun req-package-handle-loading (name f)
  (condition-case-unless-debug e
      (funcall f)
    (error (req-package--log-error (format "Unable to load package %s -- %s" name e)))))

(defun req-package-try-elpa (package)
  (let* ((ARCHIVES (if (null package-archive-contents)
                       (progn (package-refresh-contents)
                              package-archive-contents)
                     package-archive-contents))
         (AVAIL (-any? (lambda (elem)
                         (eq (car elem) package))
                       ARCHIVES))
         (INSTALLED (package-installed-p package)))
    (if (and AVAIL (not INSTALLED))
        (if (package-install package) t nil)
      INSTALLED)))

(defun req-package-try-el-get (package)
  (if req-package-el-get-present
      (let* ((AVAIL (if (el-get-recipe-filename package) t nil))
             (INSTALLED (package-installed-p package)))
        (if (and AVAIL (not INSTALLED))
            (or (el-get 'sync package) t) ;; TODO check for success
          INSTALLED))
    nil))

(defun req-package-prepare (package &optional loader)
  "prepare package - install if it is present"
  (if (not (and loader (funcall (car loader) package)))
      (-any? (lambda (elem)
               (funcall elem package))
             req-package-providers)))

(defun req-package-gen-eval (package)
  "generate eval for package and install it if present at el-get/elpa"
  (list 'use-package package))

(defun req-package-detect-cycles-traverse-impl (visited cur path)
  "traverse for cycles look up implementation"
  (puthash cur t visited)
  (if (not (-contains? path cur))
      (-each (gethash cur req-package-reqs-reversed nil)
        (lambda (dependent)
          (req-package-detect-cycles-traverse-impl visited dependent (cons cur path))))
    (progn (setq req-package-cycles-count (+ req-package-cycles-count 1))
           (req-package--log-error "cycle detected: %s" (cons cur path)))))

(defun req-package-detect-cycles-traverse (visited)
  "traverse for cycles look up"
  (maphash (lambda (key value)
             (if (null (gethash key visited nil))
                 (req-package-detect-cycles-traverse-impl visited key nil)))
           req-package-reqs-reversed)
  (if (not (eq 0 req-package-cycles-count))
      (message "%s cycle(s) detected. see M-x req-package--log-open-log"
               req-package-cycles-count)))

(defun req-package-finish ()
  "start loading process, call this after all req-package invocations"
  ;; (maphash (lambda (key value)
  ;;            (if (eq (gethash key req-package-ranks) -1)
  ;;                (progn (remhash key req-package-ranks)
  ;;                       (remhash key req-package-evals)
  ;;                       (remhash key req-package-loaders)
  ;;                       (remhash key req-package-reqs-reversed))))
  ;;          req-package-ranks)

  (progn (setq req-package-cycles-count 0)
         (req-package-detect-cycles-traverse (make-hash-table :size 200)))

  (req-package--log-debug "package requests finished: %s packages are waiting"
                          (hash-table-count req-package-ranks))

  (maphash (lambda (key value)
             (req-package-prepare key (gethash key req-package-loaders nil))
             (if (eq (gethash key req-package-ranks 0) 0)
                 (progn (puthash key -1 req-package-ranks)
                        (req-package-eval key))))
           req-package-ranks))

(put 'req-package 'lisp-indent-function 'defun)
(put 'req-package-force 'lisp-indent-function 'defun)
(put 'req-package-add-hook-execute 'lisp-indent-function 'defun)
(put 'req-package-add-hook-execute-impl 'lisp-indent-function 'defun)

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
