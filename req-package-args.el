;;; req-package-args.el --- summary:
;;; commentary:
;;; code:

(require 'dash)

(defun req-package-args-wrap (reqs)
  "Listify dependencies passed passed in REQS."
  (if (atom reqs) (list reqs) reqs))

(defun req-package-args-extract-arg (key args acc)
  "Extract KEY value from ARGS list accummulating with ACC."
  (if (null args)
      (list nil (reverse acc))
    (if (eq (car args) key)
        (list (req-package-args-wrap (car (cdr args)))
              (append (reverse acc) (cddr args)))
      (req-package-args-extract-arg key (cdr args) (cons (car args) acc)))))

(provide 'req-package-args)
;;; req-package-args ends here
