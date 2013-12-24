(require 'use-package)

(defvar req-package-targets nil
  "list of packages to load")

(defmacro req-package (name &rest params)
  `(let ((NAME ',name)
         (ARGS ',params))

     (if (eq (car ARGS) :require)
         (if (eq (cdr ARGS) nil)
             (error "invalid arguments list")
           (add-to-list 'req-package-targets
                        (list NAME (cadr ARGS) (apply 'list 'use-package NAME (cddr ARGS)))))
       (add-to-list 'req-package-targets
                    (list NAME nil (apply 'list 'use-package NAME ARGS))))))

(provide 'req-package)
