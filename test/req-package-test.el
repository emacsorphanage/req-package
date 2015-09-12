(when (require 'undercover nil t)
  (undercover "req-package.el"))

(require 'req-package)
(require 'ert)
(require 'ert-expectations)

(ert-deftest req-package-wrap-args-test ()
  (should (equal '(1) (req-package-wrap-args 1)))
  (should (equal '(1) (req-package-wrap-args '(1)))))

(ert-deftest req-package-extract-arg-test ()
  (should (equal '(6) (car (req-package-extract-arg :init '(:config 5 :init 6) nil))))
  (should (equal '(5) (car (req-package-extract-arg :config'(:config 5 :init 6) nil)))))

(ert-deftest req-package-patch-config-test ()
  (should (equal '(:config (req-package-loaded (quote foo)))
                 (req-package-patch-config 'foo '())))
  (should (equal '(:init 5 :config (req-package-loaded (quote foo)))
                 (req-package-patch-config 'foo '(:init 5)))))

(ert-deftest req-package-gen-eval-test ()
  (should (equal '(use-package bar) (req-package-gen-eval 'bar))))

(ert-deftest req-package-handle-loading-test ()
  (let ((d debug-on-error))
    (setq debug-on-error nil)
    (should (equal nil (req-package-handle-loading 'foo (lambda () (error "hi")))))
    (setq debug-on-error d)))

(expectations
 (desc "success")
 (expect 1 (req-package-handle-loading 'foo (lambda () 1))))

(provide 'req-package-test)
