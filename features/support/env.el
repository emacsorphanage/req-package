(require 'f)

(defvar req-package-support-path
  (f-dirname load-file-name))

(defvar req-package-features-path
  (f-parent req-package-support-path))

(defvar req-package-root-path
  (f-parent req-package-features-path))

(add-to-list 'load-path req-package-root-path)

(require 'req-package)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
