(defpackage #:message-stream-tests
  (:use #:cl
        #:sb-concurrency
        #:fiveam
        #:message-stream)
  (:export
   :run-tests))
