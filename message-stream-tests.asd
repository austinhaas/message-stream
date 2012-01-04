(asdf:defsystem #:message-stream-tests
  :description "Test suite for message-stream."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.1"
  :depends-on (#:sb-concurrency
               #:message-stream
               #:fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))
