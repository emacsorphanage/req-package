;;; package --- summary:
;;; commentary:
;;; code:

(defun req-package-mode-loaded-p (mode)
  "Return true if MODE is loaded now."
  (or (assoc mode minor-mode-list) (equal major-mode mode)))

(defun req-package-add-hook-execute-impl (m h f)
  "Add function F to hook H and execute it if mode M is already activated"
  (add-hook h f)
  (if (req-package-mode-loaded-p m)
      (funcall f)))

(defun req-package-add-hook-execute (m f)
  "Add function F to mode M and execute it if already activated"
  (let ((h (intern (concat (symbol-name m) "-hook"))))
    (req-package-add-hook-execute-impl m h f)))

(provide 'req-package-hooks)
;;; req-package-hooks ends here
