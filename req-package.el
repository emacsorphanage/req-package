;;; req-package.el --- A use-package wrapper for package runtime dependencies management

;; Copyright (C) 2013-2018 Edward Knyshov

;; Author: Edward Knyshov <edvorg@gmail.com>
;; Created: 25 Dec 2013
;; Version: 1.2
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
;; .. 1.3 Logging
;; .. 1.4 Migrate from use-package
;; .. 1.5 Note
;; .. 1.6 Contribute
;; .. 1.7 Changelog
;; ..... 1.7.1 `v1.1'
;; ..... 1.7.2 `v1.0'
;; ..... 1.7.3 `v0.9'
;; ..... 1.7.4 `v0.8'
;; ..... 1.7.5 `v0.7'
;; ..... 1.7.6 `v0.6'
;; ..... 1.7.7 `v0.5'
;; ..... 1.7.8 `v0.4.2'
;; ..... 1.7.9 `v0.4.1'
;; ..... 1.7.10 `v0.4-all-cycles'
;; ..... 1.7.11 `v0.3-cycles'
;; ..... 1.7.12 `v0.2-auto-fetch'


;; 1 req-package
;; ═════════════

;;   [file:https://img.shields.io/badge/license-GPL_3-green.svg]
;;   [file:http://melpa.org/packages/req-package-badge.svg]
;;   [file:http://stable.melpa.org/packages/req-package-badge.svg]
;;   [file:https://travis-ci.org/edvorg/req-package.svg]
;;   [file:https://coveralls.io/repos/edvorg/req-package/badge.svg?branch=develop&service=github]


;; [file:https://img.shields.io/badge/license-GPL_3-green.svg]
;; http://www.gnu.org/licenses/gpl-3.0.txt

;; [file:http://melpa.org/packages/req-package-badge.svg]
;; http://melpa.org/#/req-package

;; [file:http://stable.melpa.org/packages/req-package-badge.svg]
;; http://stable.melpa.org/#/req-package

;; [file:https://travis-ci.org/edvorg/req-package.svg]
;; https://travis-ci.org/edvorg/req-package

;; [file:https://coveralls.io/repos/edvorg/req-package/badge.svg?branch=develop&service=github]
;; https://coveralls.io/github/edvorg/req-package?branch=develop

;; 1.1 Description
;; ───────────────

;;   `req-package' provides dependency management for use-package.  This
;;   allows to write simple and modular configs.  Migration from
;;   use-package is simple and syntax is almost same.


;; 1.2 Usage
;; ─────────

;;   Load req-package:

;;   ┌────
;;   │ (require 'req-package)
;;   │
;;   │ (req-package el-get ;; prepare el-get (optional)
;;   │   :force t ;; load package immediately, no dependency resolution
;;   │   :config
;;   │   (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;;   │   (el-get 'sync))
;;   └────

;;   Define required packages with dependencies using `:require'.
;;   Use `:force t' if you want to avoid dependency management and load right now.
;;   Use `:el-get t' or `:el-get package-name' if you want to install from el-get.

;;   ┌────
;;   │ ;; init-dired.el
;;   │
;;   │ (req-package dired) ;; this form is optional as it doesn't have any configuration
;;   │
;;   │ (req-package dired-single
;;   │   :require dired ;; depends on dired
;;   │   :config (...))
;;   │
;;   │ (req-package dired-isearch
;;   │   :require dired ;; depends on dired
;;   │   :config (...))
;;   │
;;   │ ;; init-lua.el
;;   │
;;   │ (req-package lua-mode
;;   │   :config (...))
;;   │
;;   │ (req-package flymake-lua
;;   │   :require flymake lua-mode
;;   │   :config (...))
;;   │
;;   │ ;; init-flymake.el
;;   │
;;   │ (req-package flymake
;;   │   :config (...))
;;   │
;;   │ (req-package flymake-cursor
;;   │   :require flymake
;;   │   :config (...))
;;   │
;;   │ (req-package flymake-custom
;;   │   :require flymake
;;   │   :load-path "/path/to/file/directory"
;;   │   :config (...))
;;   └────

;;   Solve dependencies, install and load packages in right order:

;;   ┌────
;;   │ ;; order doesn't matter here
;;   │ (require 'init-dired)
;;   │ (require 'init-lua)
;;   │ (require 'init-flymake)
;;   │ (req-package-finish)
;;   └────


;; 1.3 Logging
;; ───────────

;;   You can use `req-package--log-open-log' to see, what is happening with
;;   your configuration.  You can choose log level in `req-package' group
;;   by `req-package-log-level' custom.  These log levels are supported:
;;   `fatal', `error', `warn', `info', `debug', `trace'.


;; 1.4 Migrate from use-package
;; ────────────────────────────

;;   Just replace all `(use-package ...)' with `(req-package [:require
;;   DEPS] ...)' and add `(req-package-finish)' at the end of your
;;   configuration file.  There is a `:force' keyword which simulates plain
;;   old use-package behavior.


;; 1.5 Note
;; ────────

;;   More complex req-package usage example can be found at
;;   [https://github.com/edvorg/emacs-configs].

;;   Use `load-dir' package to load all `*.el' files in a dir (e.g
;;   `~/.emacs.d/init.d')


;; 1.6 Contribute
;; ──────────────

;;   Please, pull-request your changes to `develop' branch.  Master is used
;;   for automatic *release* package builds by travis-ci.


;; 1.7 Changelog
;; ─────────────

;; 1.7.1 `v1.1'
;; ╌╌╌╌╌╌╌╌╌╌╌╌

;;   • due to use-package being mature enough drop providers system


;; 1.7.2 `v1.0'
;; ╌╌╌╌╌╌╌╌╌╌╌╌

;;   • once you called `req-package-finish' you are able reload package
;;     just by reload `req-package' form
;;   • proper errors handling. see `req-package--log-open-log' for messages
;;   • smart add-hook which invokes function if mode is loaded
;;   • refactor providers system
;;   • no need to use progn in :init and :config sections
;;   • no need to use list literal in :require section
;;   • `:loader' keyword now accepts loaders as keywords or as functions.
;;     e.g. `:el-get', `:elpa', `:built-in', `:path' and `my-loader-fn'
;;   • `req-package-force' replaced with `:force' keyword


;; 1.7.3 `v0.9'
;; ╌╌╌╌╌╌╌╌╌╌╌╌

;;   • `:loader' keyword support


;; 1.7.4 `v0.8'
;; ╌╌╌╌╌╌╌╌╌╌╌╌

;;   • bugfixes


;; 1.7.5 `v0.7'
;; ╌╌╌╌╌╌╌╌╌╌╌╌

;;   • fixed some issues with packages installation. all packages will be
;;     installed at bootstrap time
;;   • custom package providers support by `req-package-providers'
;;   • priority feature for cross provider packages loading. you can
;;     choose, what to try first - elpa, el-get, or something else


;; 1.7.6 `v0.6'
;; ╌╌╌╌╌╌╌╌╌╌╌╌

;;   • `el-get' support


;; 1.7.7 `v0.5'
;; ╌╌╌╌╌╌╌╌╌╌╌╌

;;   • Major system refactoring.
;;   • Fixed bugs with defered loading.
;;   • Significant performance optimization.
;;   • `max-specpdl-size', `max-lisp-eval-depth' issues completely solved.
;;   • Flexible `:require' keyword parsing.


;; 1.7.8 `v0.4.2'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • Bug fixes.


;; 1.7.9 `v0.4.1'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • Various tweaks and bug fixes.


;; 1.7.10 `v0.4-all-cycles'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • All cycles of your dependencies will be printed now.
;;   • Also there are more handy log messages and some bug fixes.


;; 1.7.11 `v0.3-cycles'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   • There are nice error messages about cycled dependencies now.
;;   • Cycles printed in a way: `pkg1 -> [pkg2 -> ...] pkg1'.
;;   • It means there is a cycle around `pkg1'.


;; 1.7.12 `v0.2-auto-fetch'
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

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
(req-package-bootstrap 'el-get)
(req-package-bootstrap 'dash)
(req-package-bootstrap 'log4e)
(req-package-bootstrap 'ht)

(require 'use-package)
(require 'el-get)
(require 'dash)
(require 'log4e)
(require 'ht)

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
  "Package symbol -> loaded dependencies counter.")

(defvar req-package-evals (make-hash-table :size 200 :test 'equal)
  "Package symbol -> loading code prepared for evaluation.")

(defvar req-package-branches (make-hash-table :size 200 :test 'equal))

(add-to-list 'use-package-keywords :el-get)

(defun use-package-normalize/:el-get (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((booleanp arg) name-symbol)
       ((symbolp arg) arg)
       (t
        (use-package-error
         ":el-get wants an package name or boolean value"))))))

(defun use-package-handler/:el-get (name-symbol keyword archive-name rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    ;; This happens at macro expansion time, not when the expanded code is
    ;; compiled or evaluated.
    (if (null archive-name)
        body
      (el-get-install archive-name)
      body)))

(defun req-package-patch-config (pkg form)
  "Wrap package PKG :config FORM into progn with callbacks."
  (list 'progn
        (list 'req-package-handle-loading (list 'quote pkg) (list 'lambda () form))
        (list 'req-package-loaded (list 'quote pkg))))

(defun req-package-eval-form (EVAL)
  "Logs, macroexpands and evaluates EVAL form."
  (req-package--log-trace "eval %s" EVAL)
  (eval (macroexpand-all EVAL)))

(defun req-package-eval (pkg)
  "Evaluate package PKG request."
  (let* ((DEFAULT (req-package-gen-eval pkg (list 'progn) (req-package-patch-config pkg nil) nil))
         (EVAL (gethash pkg req-package-evals DEFAULT))
         (PKG pkg))
    (req-package-handle-loading PKG (lambda () (req-package-eval-form EVAL)))))

(defun req-package-loaded (pkg)
  "Called after package PKG loaded to continue dependency graph traverse."
  (req-package--log-info "package loaded: %s" pkg)
  (let* ((EVALS (-reduce-from
                 (lambda (memo dependent)
                   (let* ((DEPS-LEFT (- (gethash dependent req-package-deps-left 0) 1)))
                     (puthash dependent DEPS-LEFT req-package-deps-left)
                     (if (equal 0 DEPS-LEFT)
                         (cons dependent memo)
                       memo)))
                 nil
                 (gethash (car pkg) req-package-required-by nil))))
    (-each EVALS (lambda (pkg)
                   (puthash pkg -1 req-package-deps-left)
                   (req-package-eval pkg)))))

(defun req-package-handle-loading (pkg f)
  "Error handle for package PKG loading process by calling F."
  (condition-case-unless-debug e
      (funcall f)
    (error (req-package--log-error (format "Unable to load package %s -- %s" pkg e)))))

(defun req-package-gen-eval (package init config rest)
  "Generate eval for PACKAGE."
  (let* ((package (car package)))
    (append (list 'use-package package)
            (list :init init)
            (list :config config)
            rest)))

(defun req-package-schedule (PKG DEPS EVAL)
  (let* ((DEPS-LEFT (gethash PKG req-package-deps-left 0))
         (BRANCHES (ht-get req-package-branches (car PKG))))
    (req-package--log-debug "package requested: %s %s" PKG EVAL)
    (puthash PKG EVAL req-package-evals)
    (ht-set req-package-branches (car PKG) (cons PKG BRANCHES))
    (if (= DEPS-LEFT -1)
        (progn ;; package already been loaded before, just eval again
          (req-package-handle-loading PKG (lambda () (req-package-eval-form EVAL)))
          DEPS-LEFT)
      (progn ;; insert package in dependency graph
        (puthash PKG 0 req-package-deps-left)
        (-each DEPS
          (lambda (req)
            (let* ((REQUIRED-BY (gethash req req-package-required-by nil))
                   (DEPS-LEFT (gethash PKG req-package-deps-left 0))
                   (REQ-DEPS-LEFT (gethash req req-package-deps-left 0))
                   (BRANCHES (ht-get req-package-branches req)))
              (ht-set req-package-branches req BRANCHES)
              (when (not (equal -1 REQ-DEPS-LEFT))
                (puthash req (cons PKG REQUIRED-BY) req-package-required-by)
                (puthash PKG (+ DEPS-LEFT 1) req-package-deps-left)))))))))

(defmacro req-package (pkg &rest args)
  "Add package PKG with ARGS to target list."
  `(let* ((PKG ',pkg)
          (ARGS ',args)
          (SPLIT1 (req-package-args-extract-arg :require ARGS nil))
          (SPLIT2 (req-package-args-extract-arg :init (cadr SPLIT1) nil))
          (SPLIT3 (req-package-args-extract-arg :config (cadr SPLIT2) nil))
          (SPLIT4 (req-package-args-extract-arg :force (cadr SPLIT3) nil))
          (DEPS (-flatten (car SPLIT1)))
          (INIT (cons 'progn (car SPLIT2)))
          (PKG (list PKG DEPS))
          (CONFIG (req-package-patch-config PKG (cons 'progn (car SPLIT3))))
          (FORCE (caar SPLIT4))
          (REST (cadr SPLIT4))
          (EVAL (req-package-gen-eval PKG INIT CONFIG REST)))
     (if FORCE
         (progn ;; load avoiding dependency management
           (req-package--log-debug "package force-requested: %s %s" PKG EVAL)
           (req-package-handle-loading PKG (lambda () (req-package-eval-form EVAL))))
       (req-package-schedule PKG DEPS EVAL))))

(defmacro req-package-force (pkg &rest args)
  `(let* ((PKG ',pkg)
          (ARGS ',args))
     (eval (macroexpand-all (apply 'list 'req-package PKG :force t ARGS)))))

(defun req-package-finish ()
  "Start loading process, call this after all req-package invocations."
  ;; (req-package-cycles-detect req-package-required-by) ;; FIXME
  (req-package--log-debug "package requests finished: %s packages are waiting"
               (hash-table-count req-package-branches))
  (maphash (lambda (req branches)
             (when (not branches)
               (let* ((REQ-PKG (list req nil))
                      (CURRENT (gethash REQ-PKG req-package-deps-left 0)))
                 (puthash REQ-PKG CURRENT req-package-deps-left))))
           req-package-branches)
  (maphash (lambda (key value)
             (when (equal (gethash key req-package-deps-left 0) 0)
               (puthash key -1 req-package-deps-left)
               (req-package-eval key)))
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
