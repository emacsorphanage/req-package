(when (require 'undercover nil t)
  (undercover "req-package.el"))

(require 'req-package)

(ert-deftest req-package-wrap-args-test ()
  (should (equal '(1) (req-package-wrap-args 1)))
  (should (equal '(1) (req-package-wrap-args '(1)))))

(provide 'req-package-test)
