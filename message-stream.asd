(asdf:defsystem #:message-stream
  :serial t
  :depends-on (#:sb-concurrency)
  :components ((:file "package")
               (:file "message-stream")))
