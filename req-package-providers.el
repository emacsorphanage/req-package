;;; req-package-providers.el --- summary:
;;; commentary:
;;; code:

(require 'ht)
(require 'dash)

(defconst req-package-providers-el-get-present
  (if (require 'el-get nil t) t nil)
  "You can check this for el get presense.")

(defcustom req-package-providers-map
  (ht ('elpa '(req-package-providers-install-elpa req-package-providers-present-elpa))
      ('el-get '(req-package-providers-install-el-get req-package-providers-present-el-get)))
  "Providers map provider -> (installer avaible-checker)."
  :group 'req-package
  :type 'list)

(defcustom req-package-providers-priority (ht ('elpa 0)
                                              ('el-get 1))
  "Priority system for package providers."
  :group 'req-package
  :type 'list)

(defun req-package-providers-get ()
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
  (let* ((INSTALLED (package-installed-p package)))
    (if (not INSTALLED)
        (if (package-install package) t nil)
      INSTALLED)))

(defun req-package-providers-present-el-get (package)
  "Return t if PACKAGE is available for installation."
  (if req-package-providers-el-get-present
      (let* ((AVAIL (if (el-get-recipe-filename package) t nil)))
        AVAIL)
    nil))

(defun req-package-providers-install-el-get (package)
  "Install PACKAGE with el-get."
  (if req-package-providers-el-get-present
      (let* ((INSTALLED (el-get-package-installed-p package)))
        (if (not INSTALLED)
            (or (el-get 'sync package) t) ;; TODO check for success
          INSTALLED))
    nil))

(defun req-package-providers-prepare (package &optional loader)
  "Prepare PACKAGE - install if it is present using LOADER if specified."
  (req-package--log-debug (format "installing package %s" package))
  (condition-case e
      (if (functionp loader)
          (funcall loader package)
        (let* ((providers (req-package-providers-get))
               (provider (if (and loader (symbolp loader))
                             loader
                           (-first (lambda (elem)
                                     (funcall (second (ht-get providers elem)) package))
                                   (-sort (lambda (a b) (< (ht-get req-package-providers-priority a -1)
                                                      (ht-get req-package-providers-priority b -1)))
                                          (ht-keys providers)))))
               (installer (first (ht-get providers provider))))
          (if installer
              (funcall installer package)
            (error (format "provider not found for package %s" package)))))
    (error (req-package--log-error (format "unable to install package %s : %s" package e)))))

(provide 'req-package-providers)
;;; req-package-providers ends here
