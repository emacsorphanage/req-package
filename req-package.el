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

(defun req-package-deps-loaded (deps evals)
  "is package already in evals eval list"
  (cond ((null deps) t)
        ((null (req-package-package-loaded (car deps) evals)) nil)
        (t (req-package-deps-loaded (cdr deps) evals))))

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
        (t (if (req-package-deps-loaded (cadar targets) evals)

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
