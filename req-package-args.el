;;; req-package-args.el --- summary:
;;; commentary:
;;; code:

(defun req-package-args-wrap (reqs)
  "Listify dependencies passed passed in REQS."
  (if (atom reqs) (list reqs) reqs))

(defun req-package-args-extract-arg (key args acc)
  "Extract KEY value from ARGS list accummulating with ACC."
  (if (null args)
      (list nil (reverse acc))
    (if (eq (first args) key)
        (list (req-package-args-wrap (first (cdr args)))
              (append (reverse acc) (cddr args)))
      (req-package-args-extract-arg key (cdr args) (cons (first args) acc)))))

(provide 'req-package-args)
;;; req-package-args ends here
