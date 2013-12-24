(require 'use-package)

(defmacro req-package (name &rest params)
  `(let ((NAME ',name)
		 (ARGS ',params))

	 (if (eq (car ARGS) :require)
		 (if (eq (cdr ARGS) nil)
			 (error "invalid arguments list")
		   (eval (apply 'list 'use-package NAME (cddr ARGS))))
	   (eval (apply 'list 'use-package NAME ARGS)))))

(provide 'req-package)
