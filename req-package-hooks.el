;;; req-package-hooks.el --- summary:
;;; commentary:
;;; code:

(defun req-package-hooks-mode-loaded-p (mode)
  "Return true if MODE is loaded now."
  (or (assoc mode minor-mode-list) (equal major-mode mode)))

(defun req-package-hooks-add-execute-impl (m h f)
  "Add function F to hook H and execute it if mode M is already activated"
  (add-hook h f)
  (if (req-package-hooks-mode-loaded-p m)
      (funcall f)))

(defun req-package-hooks-add-execute (m f)
  "Add function F to mode M and execute it if already activated"
  (let ((h (intern (concat (symbol-name m) "-hook"))))
    (req-package-hooks-add-execute-impl m h f)))

(provide 'req-package-hooks)
;;; req-package-hooks ends here
