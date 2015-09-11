(when (require 'undercover nil t)
  (undercover "*.el" (:send-report nil)))

(require 'req-package)
