(require 'req-package)
(require 'ert)
(require 'ert-expectations)

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
  (desc "req-package-args-take-args should take all arguments until next keyword")
  (expect '((1 2 :hello) (:loader 4)) (req-package-args-take-args '(1 2 :hello :loader 4) nil))
  (expect '(nil nil) (req-package-args-take-args '() nil))
  (expect '(nil (:require 4)) (req-package-args-take-args '(:require 4) nil))
  (expect '((1 2 3 4) nil) (req-package-args-take-args '(1 2 3 4) nil))
  (desc "req-package-args-extract-arg should extract value specified after keyword")
  (expect '(6) (car (req-package-args-extract-arg :init '(:config 5 :init 6) nil)))
  (expect '(5) (car (req-package-args-extract-arg :config'(:config 5 :init 6) nil)))
  (expect '(6 :hello) (car (req-package-args-extract-arg :init '(:config 5 :world :init 6 :hello) nil)))
  (expect '(:world 5) (car (req-package-args-extract-arg :config'(:config :world 5 :init 6 :hello) nil)))
  (desc "req-package-patch-config should include req-package-loaded callback in :config")
  (expect '(progn (req-package-handle-loading (quote foo) (lambda nil nil)) (req-package-loaded (quote foo)))
    (req-package-patch-config 'foo '()))
  (desc "req-package-gen-eval should generate valid use-package form")
  (expect '(use-package bar :init nil :config nil)
    (req-package-gen-eval 'bar nil nil nil))
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
  (desc "req-package-hooks-add-execute should return hook value if invoked")
  (expect "loaded"
    (with-mock
      (stub add-hook)
      (mock (req-package-hooks-mode-loaded-p 'supermode) => t)
      (req-package-hooks-add-execute-impl 'supermode 'supermode-hook (lambda () "loaded"))))
  (expect nil
    (with-mock
      (stub add-hook)
      (mock (req-package-hooks-mode-loaded-p 'supermode) => nil)
      (req-package-hooks-add-execute 'supermode (lambda () "loaded"))))
  (desc "req-package-providers-get-map is just a getter for req-package-providers-map")
  (expect req-package-providers-map (req-package-providers-get-map))
  (desc "req-package-providers-prepare should install package with recommended provider")
  (expect "package-1 installed"
    (req-package-providers-prepare 'package-1 (lambda (p) (format "%s installed" p))))
  (expect "package-2 installed"
    (with-mock
      (stub req-package-providers-get-map => (ht (:foo-provider (list (lambda (p) (format "%s installed" p))))))
      (req-package-providers-prepare 'package-2 :foo-provider)))
  (desc "req-package-providers-prepare should install package with one of req-package-providers")
  (expect "package-3 installed"
    (with-mock
      (stub req-package-providers-get-map => (ht (:foo-provider (list (lambda (p) (format "%s installed" p)) (lambda (p) t)))))
      (req-package-providers-prepare 'package-3)))
  (desc "req-package-providers-prepare should return nil if no providers found")
  (expect nil
    (with-mock
      (stub req-package-providers-get-map => (ht-create))
      (req-package-providers-prepare 'package-4))))

(provide 'req-package-test)
