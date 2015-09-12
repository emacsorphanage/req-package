(defun with-debug-on-error (m fn)
  (let* ((current debug-on-error)
         (_ (setq debug-on-error m))
         (result (funcall fn))
         (_ (setq debug-on-error current)))
    result))

(put 'with-debug-on-error 'lisp-indent-function 'defun)

(provide 'test-helper)
