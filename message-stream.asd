(asdf:defsystem #:message-stream
  :description "A simple layer over SBCL's mailbox implementation, allowing it to be used as an infinite sequence."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.1"
  :depends-on (#:sb-concurrency)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "message-stream")))))
