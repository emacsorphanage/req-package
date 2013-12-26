;;; req-package.el --- A use-package wrapper for package runtime dependencies management

;; Copyright (C) 2013 Edward Knyshov

;; Author: Edward Knyshov <edvorg@gmail.com>
;; Created: 25 Dec 2013
;; Version: 0.1
;; Package-Requires: ((use-package "1.0"))
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

;; Description

;; req-package is a macro wrapper on top of use-package.
;; It's goal is to simplify package dependencies management
;; when using use-package for your .emacs.

;; Usage

;; 1) load req-package:

;;    (require 'req-package)

;; 2) define required packages with dependencies using :require like this:

;;    (req-package dired)
;;    (req-package dired-single
;;                 :require dired
;;                 :init (...))
;;    (req-package lua-mode
;;                 :init (...))
;;    (req-package flymake)
;;    (req-package flymake-lua
;;                 :require (flymake lua-mode)
;;                 :init (...))

;; 3) to start loading packages in right order:

;;    (req-package-finish)

;; Migrate from use-package

;; Just replace all (use-package ...) with (req-package [:require DEPS] ...)
;; and add (req-package-finish) at the end of your configuration file.

;; Note

;; All use-package parameters are supported, see use-package manual for additional info.

;; Also there are possible troubles with deferred loading provided by use-package.
;; If you want to use it, try defer all packages in one dependency tree.

;;; Code:

(require 'use-package)
(require 'cl)

(defvar req-package-targets nil
  "list of packages to load")

(defvar req-package-eval-list nil
  "preprocessed list of forms to eval")

(defvar req-package-debug nil
  "if not nil, log packages loading order")

(defun req-package-wrap-reqs (reqs)
  "listify passed dependencies"
  (if (atom reqs) (list reqs) reqs))

(defmacro req-package (name &rest args)
  "add package to target list"
  `(let* ((NAME ',name)
          (ARGS ',args)
          (ERRMES "invalid arguments list")
          (HASREQ (and (not (null ARGS))
                       (eq (car ARGS) :require)
                       (if (null (cdr ARGS)) (error ERRMES) t)))
          (USEPACKARGS (if HASREQ (cddr ARGS) ARGS))
          (REQS (if HASREQ (req-package-wrap-reqs (cadr ARGS)) nil))
          (TARGET (list NAME REQS (apply 'list 'use-package NAME USEPACKARGS))))

     (add-to-list 'req-package-targets TARGET)))

(defun req-package-package-loaded (dep evals)
  "is package already in evals eval list"
  (cond ((null evals) nil)
        ((eq (cadar evals) dep) t)
        (t (req-package-package-loaded dep (cdr evals)))))

(defun req-package-packages-loaded (deps evals)
  "is package already in evals eval list"
  (cond ((null deps) t)
        ((null (req-package-package-loaded (car deps) evals)) nil)
        (t (req-package-packages-loaded (cdr deps) evals))))

(defun req-package-form-eval-list (targets skipped evals skippederr)
  "form eval list form target list"
  (cond ((null targets) (if (null skipped)

                            ;; there're no packages skipped,
                            ;; just return collected data
                            evals

                          (if skippederr

                              ;; some packages were skipped
                              ;; we did everything we could
                              (error "some packages will never be loaded. you need to fix dependencies")

                            ;; some package were skipped
                            ;; try to load it now again
                            (req-package-form-eval-list skipped
                                                        nil
                                                        evals
                                                        t))))

        ;; if there is no dependencies
        ((null (cadar targets)) (req-package-form-eval-list (cdr targets)
                                                            skipped
                                                            (cons (caddar targets) evals)
                                                            nil))

        ;; there are some dependencies, lets look what we can do with it
        (t (if (req-package-packages-loaded (cadar targets) evals)

               ;; all required packages loaded
               (req-package-form-eval-list (cdr targets)
                                           skipped
                                           (cons (caddar targets) evals)
                                           nil)

             ;; some of required packages not loaded
             (req-package-form-eval-list (cdr targets)
                                         (cons (car targets) skipped)
                                         evals
                                         skippederr)))))

(defun req-package-eval (list)
  "evaluate preprocessed list"
  (mapcar (lambda (target) (progn (if req-package-debug
                                 (print (concat "loading " (symbol-name (cadr target))))
                               nil)
                             (eval target)))
          list))

(defun req-package-finish ()
  "start loading process, call this after all req-package invocations"
  (progn (setq req-package-eval-list (reverse (req-package-form-eval-list req-package-targets nil nil nil)))
         (req-package-eval req-package-eval-list)))

(provide 'req-package)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; req-package.el ends here
