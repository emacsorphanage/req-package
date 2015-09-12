(when (require 'undercover nil t)
  (undercover "req-package.el"))

(require 'req-package)

(ert-deftest req-package-wrap-args-test ()
  (should (equal '(1) (req-package-wrap-args 1)))
  (should (equal '(1) (req-package-wrap-args '(1)))))

(ert-deftest req-package-extract-arg-test ()
  (should (equal '(6) (car (req-package-extract-arg :init '(:config 5 :init 6) nil))))
  (should (equal '(5) (car (req-package-extract-arg :config'(:config 5 :init 6) nil)))))

(provide 'req-package-test)
