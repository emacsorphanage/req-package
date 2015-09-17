(provide 'test-helper)

(when (require 'undercover nil t)
  (undercover "req-package.el"))
