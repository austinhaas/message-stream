(defpackage #:message-stream
  (:use #:cl)
  (:export
   :*default-timeout*
   :timeout-condition
   :make-message-stream
   :stream-cons
   :stream-car
   :stream-cdr
   :stream-nth
   :stream-find
   :stream-find-if
   :stream-remove
   :stream-remove-if
   :stream-dequeue))
