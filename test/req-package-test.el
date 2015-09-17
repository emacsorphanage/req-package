(require 'req-package)
(require 'ert)
(require 'ert-expectations)

(when (require 'undercover nil t)
  (undercover "req-package.el"))

(defun with-debug-on-error (m fn)
  (let* ((current debug-on-error)
         (_ (setq debug-on-error m))
         (result (funcall fn))
         (_ (setq debug-on-error current)))
    result))

(put 'with-debug-on-error 'lisp-indent-function 'defun)

(expectations
  (desc "with-debug-on-error should preserve required debug mode")
  (expect t (with-debug-on-error t (lambda () debug-on-error)))
  (expect nil (with-debug-on-error nil (lambda () debug-on-error)))
  (expect t (let* ((before debug-on-error)
                   (_ (with-debug-on-error nil (lambda () nil)))
                   (after debug-on-error))
              (equal before after)))
  (expect t (let* ((before debug-on-error)
                   (_ (with-debug-on-error t (lambda () nil)))
                   (after debug-on-error))
              (equal before after)))
  (desc "with-debug-on-error should return function call value")
  (expect "foo" (with-debug-on-error nil (lambda () "foo")))
  (desc "req-package-wrap-args should wrap atom in list")
  (expect '(1) (req-package-wrap-args 1))
  (desc "req-package-wrap-args should do nothing with list")
  (expect '(1) (req-package-wrap-args '(1)))
  (desc "req-package-extract-arg should extract value specified after keyword")
  (expect '(6) (car (req-package-extract-arg :init '(:config 5 :init 6) nil)))
  (expect '(5) (car (req-package-extract-arg :config'(:config 5 :init 6) nil)))
  (desc "req-package-patch-config should include req-package-loaded callback in :config")
  (expect '(:config (req-package-loaded (quote foo)))
    (req-package-patch-config 'foo '()))
  (expect '(:init 5 :config (req-package-loaded (quote foo)))
    (req-package-patch-config 'foo '(:init 5)))
  (expect '(:init 5 :config (progn (req-package-handle-loading (quote foo) (lambda nil (print "hello world"))) (req-package-loaded (quote foo))))
    (req-package-patch-config 'foo '(:init 5 :config (print "hello world"))))
  (desc "req-package-gen-eval should generate valid use-package form")
  (expect '(use-package bar) (req-package-gen-eval 'bar))
  (desc "req-package-handle-loading should return eval value")
  (expect 1
    (req-package-handle-loading 'foo (lambda () 1)))
  (desc "req-package-handle-loading should return nil in case of error")
  (expect nil
    (with-debug-on-error nil
      (lambda ()
        (req-package-handle-loading 'foo (lambda () (error "hi"))))))
  (desc "req-package-eval should return eval result")
  (expect "joker"
    (with-mock
      (stub gethash => "joker")
      (req-package-eval 'evil)))
  (desc "req-package-add-hook-execute should return hook value if invoked")
  (expect "loaded"
    (with-mock
      (stub add-hook)
      (mock (req-package-mode-loaded-p 'supermode) => t)
      (req-package-add-hook-execute-impl 'supermode 'supermode-hook (lambda () "loaded"))))
  (expect nil
    (with-mock
      (stub add-hook)
      (mock (req-package-mode-loaded-p 'supermode) => nil)
      (req-package-add-hook-execute 'supermode (lambda () "loaded"))))
  (desc "req-package-get-providers is just a getter for req-package-providers")
  (expect req-package-providers (req-package-get-providers))
  (desc "req-package-prepare should install package with recommended provider")
  (expect "package-1 installed"
    (req-package-prepare 'package-1 (lambda (p) (format "%s installed" p))))
  (expect "package-2 installed"
    (with-mock
      (stub req-package-get-providers => (ht ('foo-provider (list (lambda (p) (format "%s installed" p))))))
      (req-package-prepare 'package-2 'foo-provider)))
  (desc "req-package-prepare should install package with one of req-package-providers")
  (expect "package-3 installed"
    (with-mock
      (stub req-package-get-providers => (ht ('foo-provider (list (lambda (p) (format "%s installed" p)) (lambda (p) t)))))
      (req-package-prepare 'package-3)))
  (desc "req-package-prepare should return nil if no providers found")
  (expect nil
    (with-mock
      (stub req-package-get-providers => (ht-create))
      (req-package-prepare 'package-4))))

(provide 'req-package-test)
