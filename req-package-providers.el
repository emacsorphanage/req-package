;;; req-package-providers.el --- summary:
;;; commentary:
;;; code:

(require 'ht)
(require 'dash)
(require 'package)

(defcustom req-package-providers-map
  (ht (:elpa '(req-package-providers-install-elpa req-package-providers-present-elpa))
      (:el-get '(req-package-providers-install-el-get req-package-providers-present-el-get))
      (:built-in '(req-package-providers-install-built-in req-package-providers-present-built-in))
      (:path '(req-package-providers-install-path req-package-providers-present-path)))
  "Providers map provider -> (installer available-checker)."
  :group 'req-package
  :type 'list)

(defcustom req-package-providers-priority
  (ht (:elpa 0)
      (:el-get 1)
      (:built-in 2)
      (:path 3))
  "Priority system for package providers."
  :group 'req-package
  :type 'list)


(defvar req-package-paths (make-hash-table :size 200 :test 'equal)
  "Package symbol -> custom load path.")

(defun req-package-providers-get-map ()
  "Just get package providers list."
  req-package-providers-map)

(defun req-package-providers-present-elpa (package)
  "Return t if PACKAGE is available for installation."
  (let* ((ARCHIVES (if (null package-archive-contents)
                       (progn (package-refresh-contents)
                              package-archive-contents)
                     package-archive-contents))
         (AVAIL (-any? (lambda (elem)
                         (eq (car elem) package))
                       ARCHIVES)))
    AVAIL))

(defun req-package-providers-install-elpa (package)
  "Install PACKAGE with elpa."
  (let ((INSTALLED (assq package package-alist)))
    (if (not INSTALLED)
        (let* ((PKG (second (assq package package-archive-contents))))
          (req-package--log-info (format "installing package %s" package))
          (package-install PKG))
      INSTALLED)))

(defun req-package-providers-el-get-present ()
  (if (require 'el-get nil t) t nil))

(defun req-package-providers-present-el-get (package)
  "Return t if PACKAGE is available for installation."
  (when (req-package-providers-el-get-present)
    (el-get-recipe-filename package)))

(defun req-package-providers-install-el-get (package)
  "Install PACKAGE with el-get."
  (if (req-package-providers-el-get-present)
      (let* ((INSTALLED (el-get-package-installed-p package)))
        (if (not INSTALLED)
            (progn
              (req-package--log-info (format "installing package %s" package))
              (el-get 'sync package))
          INSTALLED))
    (req-package--log-error "can not find el-get to install package %s" package)))

(defun req-package-providers-present-built-in (package)
  (package-built-in-p package))

(defun req-package-providers-install-built-in (package)
  (unless (package-built-in-p package)
    (error "package is not built-in")))

(defun req-package--load-path (package)
  (append (ht-get req-package-paths package nil) load-path))

(defun req-package-providers-present-path (package)
  (locate-file (symbol-name package) (req-package--load-path package) '(".el" ".elc")))

(defun req-package-providers-install-path (package)
  (unless (req-package-providers-present-path package)
     (error "package is not on load path")))

(defun req-package-providers-prepare (package &optional loader)
  "Prepare PACKAGE - install if it is present using LOADER if specified."
  (condition-case e
      (if (functionp loader)
          (funcall loader package)
        (let* ((providers (req-package-providers-get-map))
               (provider (if (and loader (keywordp loader))
                             loader
                           (-first (lambda (elem)
                                     (funcall (second (ht-get providers elem)) package))
                                   (-sort (lambda (a b) (< (ht-get req-package-providers-priority a -1)
                                                           (ht-get req-package-providers-priority b -1)))
                                          (ht-keys providers)))))
               (installer (car (ht-get providers provider))))
          (if installer
              (funcall installer package)
            (when (not (require package nil t))
              (error "cannot prepare package %s. no provider, no build-in, no file on load-path" package)))))
    (error (req-package--log-error (format "unable to install package %s : %s" package e)))))

(provide 'req-package-providers)
;;; req-package-providers ends here
