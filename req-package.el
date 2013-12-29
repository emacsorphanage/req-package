;;; req-package.el --- A use-package wrapper for package runtime dependencies management

;; Copyright (C) 2013 Edward Knyshov

;; Author: Edward Knyshov <edvorg@gmail.com>
;; Created: 25 Dec 2013
;; Version: 0.2
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

;;    Just replace all (use-package ...) with (req-package [:require DEPS] ...)
;;    and add (req-package-finish) at the end of your configuration file.

;; Note

;;    All use-package parameters are supported, see use-package manual for additional info.

;;    Also there are possible troubles with deferred loading provided by use-package.
;;    If you want to use it, try defer all packages in one dependency tree.

;; Changelog:

;;    v0.2-auto-fetch:
;;       There is no need of explicit :ensure in your code now.
;;       When you req-package it adds :ensure if package is available in your repos.
;;       Also package deps :ensure'd automatically too.
;;       Just write (req-package pkg1 :require pkg2) and all you need will be installed.

;;; Code:

(require 'use-package)
(require 'cl)

(defvar req-package-targets nil
  "list of packages to load")

(defvar req-package-eval-list nil
  "preprocessed list of forms to eval")

(defvar req-package-verbose nil
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
          (TARGET (req-package-gen-target NAME REQS USEPACKARGS)))

     (add-to-list 'req-package-targets TARGET)))

(defun req-package-package-targeted (dep targets)
  "returns nil if package is in targets or (dep) if not"
  (cond ((null targets) (list dep))
        ((eq (caar targets) dep) nil)
        (t (req-package-package-targeted dep (cdr targets)))))

(defun req-package-packages-targeted (deps targets)
  "returns nil if packages are in targets or list of missing packages"
  (cond ((null deps) nil)
        (t (let ((headdeps (req-package-package-targeted (car deps) targets))
                 (taildeps (req-package-packages-targeted (cdr deps) targets)))
             (append headdeps taildeps)))))

(defun req-package-package-loaded (dep evals)
  "returns nil if package is in evals or (dep) if not"
  (cond ((null evals) (list dep))
        ((eq (cadar evals) dep) nil)
        (t (req-package-package-loaded dep (cdr evals)))))

(defun req-package-packages-loaded (deps evals)
  "returns nil if packages are in evals or list of missing packages"
  (cond ((null deps) nil)
        (t (let ((headdeps (req-package-package-loaded (car deps) evals))
                 (taildeps (req-package-packages-loaded (cdr deps) evals)))
             (append headdeps taildeps)))))

(defun req-package-form-eval-list (alltargets targets skipped evals err)
  "form eval list form target list"
  (cond ((null targets) (if (null skipped)

                            ;; there're no packages skipped,
                            ;; just return collected data
                            evals

                          (if err

                              ;; some packages were skipped
                              ;; try to handle it
                              (req-package-error-cycled-deps (symbol-name (caar skipped)))

                            ;; some package were skipped
                            ;; try to load it now again
                            (req-package-form-eval-list alltargets
                                                        skipped
                                                        nil
                                                        evals
                                                        t))))

        ;; if there is no dependencies
        ((null (cadar targets)) (req-package-form-eval-list alltargets
                                                            (cdr targets)
                                                            skipped
                                                            (cons (caddar targets)
                                                                  evals)
                                                            nil))

        ;; there are some dependencies, lets look what we can do with it
        (t (let* ((notloaded1 (req-package-packages-loaded (cadar targets) evals))
                  (nottargeted1 (req-package-packages-targeted notloaded1 alltargets)))

             (if (null notloaded1)

                 ;; all required packages loaded
                 (req-package-form-eval-list alltargets
                                             (cdr targets)
                                             skipped
                                             (cons (caddar targets)
                                                   evals)
                                             nil)

               ;; some deps not loaded
               (if nottargeted1

                   ;; some deps not targeted, auto load
                   (let* ((newtargets (req-package-gen-targets nottargeted1)))
                     (req-package-form-eval-list (append newtargets
                                                         alltargets)
                                                 (append newtargets
                                                         targets)
                                                 skipped
                                                 evals
                                                 nil))

                 ;; some of required packages is targeted but not loaded
                 (req-package-form-eval-list alltargets
                                             (cdr targets)
                                             (cons (car targets) skipped)
                                             evals
                                             err)))))))

(defun req-package-error-cycled-deps (package)
  (error (format "%s: cycled dependencies" package)))

(defun req-package-gen-eval (package)
  "generates eval for package. if it is available in repo, try to fetch it"
  (let* ((ARCHIVES (cond ((null package-archive-contents) (progn (package-refresh-contents)
                                                                 package-archive-contents))
                         (t package-archive-contents)))
         (AVAIL (some (lambda (elem)
                        (eq (car elem) package))
                      ARCHIVES))
         (EVAL (cond (AVAIL (list 'use-package package ':ensure package))
                     (t (list 'use-package package)))))
    EVAL))

(defun req-package-gen-evals (packages)
  "extends evals with packages"
  (cond (packages (let* ((tail (req-package-gen-evals (cdr packages))))
                    (cons (req-package-gen-eval (car packages)) tail)))
        (t nil)))

(defun req-package-gen-target (package reqs useargs)
  "generates target for package. if it is available in repo, try to fetch it"
  (list package reqs (append (req-package-gen-eval package) useargs)))

(defun req-package-gen-targets (packages)
  "extends targets with packages"
  (cond (packages (let* ((tail (req-package-gen-targets (cdr packages))))
                    (cons (req-package-gen-target (car packages) nil nil) tail)))
        (t nil)))

(defun req-package-eval (list)
  "evaluate preprocessed list"
  (mapcar (lambda (target) (progn (if req-package-verbose
                                 (print (concat "loading "
                                                (symbol-name (cadr target))))
                               nil)
                             (eval target)))
          list))

(defun req-package-finish ()
  "start loading process, call this after all req-package invocations"
  (progn (setq req-package-eval-list
               (reverse (req-package-form-eval-list req-package-targets
                                                    req-package-targets
                                                    nil
                                                    nil
                                                    nil)))
         (req-package-eval req-package-eval-list)))

(provide 'req-package)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; req-package.el ends here
