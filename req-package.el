;;; req-package.el --- A use-package wrapper for package runtime dependencies management

;; Copyright (C) 2013 Edward Knyshov

;; Author: Edward Knyshov <edvorg@gmail.com>
;; Created: 25 Dec 2013
;; Version: 0.4
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
;; It's goal is to simplify package dependencies management,
;; when using use-package for your .emacs.

;; Usage

;; 1) Load req-package:

;;    (require 'req-package)

;; 2) Define required packages with dependencies using :require like this:

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

;; 3) To start loading packages in right order:

;;    (req-package-finish)

;; Migrate from use-package

;;    Just replace all (use-package ...) with (req-package [:require DEPS] ...)
;;    and add (req-package-finish) at the end of your configuration file.

;; Note

;;    All use-package parameters are supported, see use-package manual for additional info.

;;    Also there are possible troubles with deferred loading provided by use-package.
;;    If you want to use it, try defer all packages in one dependency tree.

;; Changelog:

;;    v0.3-cycles
;;       There are nice error messages about cycled dependencies now.
;;       Cycles printed in a way: pkg1 -> [pkg2 -> ...] pkg1.
;;       It means there is a cycle around pkg1.
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

(defvar req-package-verbose nil
  "if not nil, log packages loading order")

(defun req-package-wrap-reqs (reqs)
  "listify passed dependencies"
  (if (atom reqs) (list reqs) reqs))

(defun req-package-log (err visprefix mes)
  (let* ((prefix "req-package: ")
         (noprefix "             ")
         (splitted (split-string mes "\n"))
         (titleprefix (if visprefix prefix noprefix))
         (title (concat titleprefix (car splitted)))
         (description (mapcar (lambda (line)
                                (concat "\n" noprefix line))
                              (cdr splitted)))
         (fullmes (apply 'concat "" (cons title description))))
    (if err
        (error fullmes)
      (print fullmes))))

(defmacro req-package (name &rest args)
  "add package to target list"
  `(let* ((NAME ',name)
          (ARGS ',args)
          (ERRMES "invalid arguments list")
          (HASREQ (and (not (null ARGS))
                       (eq (car ARGS) :require)
                       (if (null (cdr ARGS))
                           (req-package-log t
                                            t
                                            ERRMES)
                         t)))
          (USEPACKARGS (if HASREQ (cddr ARGS) ARGS))
          (REQS (if HASREQ (req-package-wrap-reqs (cadr ARGS)) nil))
          (TARGET (req-package-gen-target NAME REQS USEPACKARGS)))

     (add-to-list 'req-package-targets TARGET)))

(defun req-package-package-targeted (dep targets)
  "return nil if package is in targets or (dep) if not"
  (cond ((null targets) (list dep))
        ((eq (caar targets) dep) nil)
        (t (req-package-package-targeted dep (cdr targets)))))

(defun req-package-packages-targeted (deps targets)
  "return nil if packages are in targets or list of missing packages"
  (cond ((null deps) nil)
        (t (let ((headdeps (req-package-package-targeted (car deps) targets))
                 (taildeps (req-package-packages-targeted (cdr deps) targets)))
             (append headdeps taildeps)))))

(defun req-package-package-loaded (dep evals)
  "return nil if package is in evals or (dep) if not"
  (cond ((null evals) (list dep))
        ((eq (cadar evals) dep) nil)
        (t (req-package-package-loaded dep (cdr evals)))))

(defun req-package-packages-loaded (deps evals)
  "return nil if packages are in evals or list of missing packages"
  (cond ((null deps) nil)
        (t (let ((headdeps (req-package-package-loaded (car deps) evals))
                 (taildeps (req-package-packages-loaded (cdr deps) evals)))
             (append headdeps taildeps)))))

(defun req-package-form-eval-list (alltargets targets skipped evals err)
  "form eval list from target list"
  (cond ((null targets) (if (null skipped)

                            ;; there're no packages skipped,
                            ;; just return collected data
                            evals

                          (if err

                              ;; some packages were skipped
                              ;; try to handle it
                              (req-package-error-cycled-deps skipped
                                                             nil
                                                             nil)

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
        (t (let* ((notloaded1 (req-package-packages-loaded (cadar targets) evals)))

             (if (null notloaded1)

                 ;; all required packages loaded
                 (req-package-form-eval-list alltargets
                                             (cdr targets)
                                             skipped
                                             (cons (caddar targets)
                                                   evals)
                                             nil)

               ;; some deps not loaded
               (let* ((nottargeted1 (req-package-packages-targeted notloaded1 alltargets)))
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
                                               err))))))))

(defun req-package-find-target (name targets)
  "find target in targets by name"
  (cond ((null targets) nil)
        ((eq (caar targets) name) (car targets))
        (t (req-package-find-target name (cdr targets)))))

(defun req-package-cut-cycle (target cycle)
  "remove unnecessary cycle tail"
  (cond ((null cycle) cycle)
        ((eq (car target) (caar cycle)) cycle)
        (t (req-package-cut-cycle target (cdr cycle)))))

(defun req-package-find-cycle (current path graph)
  "find cycle in graph starting from current. return cycle path if found or nil"
  (let* ((deps (cadr current)))
    (if (null deps)
        nil
      (let* ((firstdep (car deps))
             (depvisited (req-package-find-target firstdep path)))
        (if depvisited
            (req-package-cut-cycle depvisited
                                   (reverse (cons depvisited path)))
          (let* ((firstdep (req-package-find-target firstdep graph))
                 (cycle (req-package-find-cycle firstdep
                                                (cons firstdep path)
                                                graph)))
            (if cycle
                cycle
              (req-package-find-cycle (list (car current)
                                            (cdr deps))
                                      path
                                      graph))))))))

(defun req-package-simplify-cycle (cycle)
  "remove unnecessary information from cycle and return it back"
  (if (null cycle)
      cycle
    (cons (caar cycle)
          (req-package-simplify-cycle (cdr cycle)))))

(defun req-package-find-cycles (from graph)
  "find cycles in graph starting from current. return cycles if found or nil"
  (if (null (cadr from))
      nil
    (let* ((head (req-package-simplify-cycle (req-package-find-cycle from
                                                                     (list from)
                                                                     graph)))
           (newfrom (list (car from) (cdadr from)))
           (tail (req-package-find-cycles newfrom graph)))
      (if head
          (cons head tail)
        tail))))

(defun req-package-cycle-shift (cycle)
  "shift cycle to left"
  (if (null cycle)
      cycle
    (let* ((head (car cycle))
           (tail (cdr cycle))
           (mid (reverse (cdr (reverse tail)))))
      (if (null mid)
          (list head head)
        (append mid
                (list head
                      (car mid)))))))

(defun req-package-cycles-equal-simple (cycle1 cycle2 variants)
  "return t if cycles is identical or nil if not. not working if cycle1 is reversed cycle2"
  (cond ((and (null cycle1) (null cycle2)) t)
        ((or (null cycle1) (null cycle2)) nil)
        ((null variants) nil)
        (t (if (eq (car cycle1) (car cycle2))
               (req-package-cycles-equal-simple (cdr cycle1)
                                                (cdr cycle2)
                                                (cdr cycle2))
             (req-package-cycles-equal-simple cycle1
                                              (req-package-cycle-shift cycle2)
                                              (cdr variants))))))

(defun req-package-cycles-equal (cycle1 cycle2)
  "return t if cycles is identical or nil if not"
  (let* ((reversed (reverse cycle2)))
    (or (req-package-cycles-equal-simple cycle1
                                         cycle2
                                         cycle2)
        (req-package-cycles-equal-simple cycle1
                                         reversed
                                         reversed))))

(defun req-package-remove-cycle-duplicates (duplicateof cycles)
  "remove duplicates of cycle from cycles"
  (if (null cycles)
      cycles
    (let* ((tail (req-package-remove-cycle-duplicates duplicateof
                                                      (cdr cycles))))
      (if (req-package-cycles-equal duplicateof
                                    (car cycles))
          tail
        (cons (car cycles) tail)))))

(defun req-package-remove-cycles-duplicates (duplicatesof cycles)
  "remove duplicates of cycles from cycles"
  (if (null cycles)
      cycles
    (if (null duplicatesof)
        cycles
      (let* ((duplicateof (car duplicatesof))
             (newcycles (cons duplicateof
                              (req-package-remove-cycle-duplicates duplicateof
                                                                   cycles))))
        (req-package-remove-cycles-duplicates (cdr duplicatesof)
                                              newcycles)))))

(defun req-package-cycle-string (cycle)
  "convert cycle to string"
  (cond ((null cycle) "")
        (t (concat " -> "
                   (symbol-name (car cycle))
                   (req-package-cycle-string (cdr cycle))))))

(defun req-package-cycles-string (cycles)
  "convert cycles to string"
  (cond ((null cycles) "")
        (t (concat "\n"
                   (if (null (car cycles))
                       ""
                     (substring (req-package-cycle-string (car cycles)) 4))
                   (req-package-cycles-string (cdr cycles))))))

(defun req-package-error-cycled-deps (skipped before cycles)
  "compute info about error and print corresponding message"
  (if (null skipped)
      (let* ((filtered (req-package-remove-cycles-duplicates cycles
                                                             cycles)))
        (req-package-log t
                         t
                         (concat "cycled deps:"
                                 (req-package-cycles-string filtered))))
    (let* ((newcycles (req-package-find-cycles (car skipped)
                                               (append before
                                                       (cdr skipped)))))
      (req-package-error-cycled-deps (cdr skipped)
                                     (cons (car skipped)
                                           before)
                                     (append newcycles cycles)))))

(defun req-package-gen-eval (package)
  "generate eval for package. if it is available in repo, add :ensure keyword"
  (let* ((ARCHIVES (if (null package-archive-contents)
                       (progn (package-refresh-contents)
                              package-archive-contents)
                     package-archive-contents))
         (AVAIL (some (lambda (elem)
                        (eq (car elem) package))
                      ARCHIVES))
         (EVAL (cond (AVAIL (list 'use-package package ':ensure package))
                     (t (list 'use-package package)))))
    EVAL))

(defun req-package-gen-evals (packages)
  "generate evals for packages"
  (cond (packages (let* ((tail (req-package-gen-evals (cdr packages))))
                    (cons (req-package-gen-eval (car packages)) tail)))
        (t nil)))

(defun req-package-gen-target (package reqs useargs)
  "generate target for package. if it is available in repo, try to fetch it"
  (list package reqs (append (req-package-gen-eval package) useargs)))

(defun req-package-gen-targets (packages)
  "generate targets for packages"
  (cond (packages (let* ((tail (req-package-gen-targets (cdr packages))))
                    (cons (req-package-gen-target (car packages) nil nil) tail)))
        (t nil)))

(defun req-package-eval (evals verbose)
  "evaluate eval list and print message if verbose is not nil"
  (mapcar (lambda (target) (progn (if verbose
                                 (req-package-log nil
                                                  t
                                                  (concat "loading "
                                                          (symbol-name (cadr target))))
                               nil)
                             (eval target)))
          evals))

(defun req-package-finish ()
  "start loading process, call this after all req-package invocations"
  (let* ((targets req-package-targets)
         (evals (reverse (req-package-form-eval-list targets
                                                     targets
                                                     nil
                                                     nil
                                                     nil))))
    (progn (setq req-package-targets nil)
           (req-package-eval evals req-package-verbose))))

(provide 'req-package)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; req-package.el ends here
