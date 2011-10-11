(asdf:defsystem #:message-stream-tests
  :serial t
  :depends-on (#:sb-concurrency
               #:message-stream
               #:fiveam)
  :components ((:file "test-package")
               (:file "tests")))
