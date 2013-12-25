(require 'use-package)
(require 'cl)

(defvar req-package-targets nil
  "list of packages to load")

(defvar req-package-eval-list nil
  "preprocessed list of forms to eval")

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
  (cond ((null evals) nil)
        ((eq (cadar evals) dep) t)
        (t (req-package-package-loaded dep (cdr evals)))))

(defun req-package-deps-loaded (deps evals)
  (cond ((null deps) t)
        ((null (req-package-package-loaded (car deps) evals)) nil)
        (t (req-package-deps-loaded (cdr deps) evals))))

(defun req-package-form-eval-list (targets skipped)
  "form eval list form target list"
  (cond ((null targets) nil)

        ;; if there is no dependencies
        ((null (cadar targets)) (cons (caddar targets)
                                      (req-package-form-eval-list (cdr targets) skipped)))

        ;; there are some dependencies, lets look what we can do with it
        (t (cons (caddar targets)
                 (req-package-form-eval-list (cdr targets) skipped)))))

(defun req-package-eval (list)
  "evaluate preprocessed list"
  (mapcar (lambda (target) (eval target))
          list))

(defun req-package-finish ()
  "start loading process, call this after all req-package invocations"
  (progn (setq req-package-eval-list (reverse (req-package-form-eval-list req-package-targets nil)))
         (req-package-eval req-package-eval-list)))

(provide 'req-package)
