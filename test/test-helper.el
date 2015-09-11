(when (require 'undercover nil t)
  (undercover "req-package.el" (:send-report nil)))

(require 'req-package)
