;;; req-package-args.el --- summary:
;;; commentary:
;;; code:

(require 'dash)

(defconst req-package-keywords '(:loader :require))

(defun req-package-args-take-args (args acc)
  "Take all args until next keyword."
  (cond ((or (null args)
             (and (keywordp (car args))
                  (or (-contains? use-package-keywords (car args))
                      (-contains? req-package-keywords (car args))))) (list (reverse acc) args))
        (t (req-package-args-take-args (cdr args) (cons (car args) acc)))))

(defun req-package-args-extract-arg (key args acc)
  "Extract KEY value from ARGS list accummulating with ACC."
  (if (null args)
      (list nil (reverse acc))
    (if (eq (car args) key)
        (let* ((REST (cdr args))
               (ALL (req-package-args-take-args REST nil))
               (KEY-ARGS (car ALL))
               (REST-ARGS (cadr ALL)))
          (list KEY-ARGS
                (append (reverse acc) REST-ARGS)))
      (req-package-args-extract-arg key (cdr args) (cons (car args) acc)))))

(provide 'req-package-args)
;;; req-package-args ends here
