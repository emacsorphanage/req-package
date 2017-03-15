;;; req-package-cycles.el --- summary:
;;; commentary:
;;; code:

(require 'dash)

(defvar req-package-cycles-count 0
  "Number of cycles detected.")

(defun req-package-cycles-detect-traverse-impl (graph visited cur path)
  "Traverse for cycles look up implementation"
  (puthash cur t visited)
  (if (not (-contains? path cur))
      (-each (gethash cur graph nil)
        (lambda (dependent)
          (req-package-cycles-detect-traverse-impl graph visited dependent (cons cur path))))
    (progn (setq req-package-cycles-count (+ req-package-cycles-count 1))
           (req-package--log-error "cycle detected: %s" (cons cur path)))))

(defun req-package-cycles-detect-traverse (graph visited)
  "Traverse for cycles look up"
  (maphash (lambda (key value)
             (if (null (gethash key visited nil))
                 (req-package-cycles-detect-traverse-impl graph visited key nil)))
           graph)
  (if (not (eq 0 req-package-cycles-count))
      (message "%s cycle(s) detected. see M-x req-package--log-open-log"
               req-package-cycles-count)))

(defun req-package-cycles-detect (graph)
  (setq req-package-cycles-count 0)
  (req-package-cycles-detect-traverse graph (make-hash-table :size 200)))

(provide 'req-package-cycles)
;;; req-package-cycles ends here
