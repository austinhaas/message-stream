(in-package #:message-stream)

(defparameter *default-timeout* 5000
  "The default amount of time, in milliseconds, that an operation will
run before signalling a timeout-condition.")
(defparameter *sleep-interval* 200
  "The default interval, in milliseconds, that the
receive-message-with-timeout function will sleep between repeated
attempts to retrieve the next message.")

(define-condition timeout-condition (condition)
  ()
  (:documentation "Signalled when an operation times out."))

(defparameter *stop-time* nil
  "This variable is bound automatically via the with-timeout macro.")

(defmacro with-timeout (timeout exp)
  `(let ((*stop-time* (when ,timeout (+ (get-internal-real-time) ,timeout))))
     ,exp))

(defun receive-message-with-timeout (mailbox timeout &optional (sleep-interval *sleep-interval*))
  "Tries to retrieve a message from mailbox, blocking until timeout if
necessary. Returns two values: the message retrieved, or nil if the
operation timed out, and a boolean indicating if the operation was
successful or not. The value of timeout is specified in
milliseconds. If timeout is nil, then it is assumed to be infinity. A
negative timeout is the same as 0."
  ;; I'll change this crude implementation once SBCL has better
  ;; support for timeouts.
  (cond ((null timeout)
         (values (sb-concurrency:receive-message mailbox) t))
        ((<= timeout 0)
         (sb-concurrency:receive-message-no-hang mailbox))
        (t
         (let ((time-remaining timeout))
           (loop
             (multiple-value-bind (message success)
                 (sb-concurrency:receive-message-no-hang mailbox)
               (when success
                 (return (values message t)))
               (when (< time-remaining 0)
                 (return (values nil nil)))
               (sleep (/ sleep-interval 1000))
               (decf time-remaining sleep-interval)))))))

(defmacro stream-cons (a b)
  ;; We have to delay the car, as well as the cdr, because we don't
  ;; want to pull any message until it is required.
  `(cons (delay ,a) (delay ,b)))

(declaim (inline %stream-car))
(defun %stream-car (stream)
  (force (car stream)))

(defun stream-car (stream &optional (timeout *default-timeout*))
  (with-timeout timeout (%stream-car stream)))

(declaim (inline %stream-cdr))
(defun %stream-cdr (stream)
  ;; We must ensure that the car is called before the cdr due to the
  ;; nature of our particular use-case: retrieving messages in order
  ;; from a FIFO queue.
  (force (car stream))
  (force (cdr stream)))

(defun stream-cdr (stream &optional (timeout *default-timeout*))
  (with-timeout timeout (%stream-cdr stream)))

(defmacro delay (exp)
  `(memo-proc (lambda () ,exp)))

(defun force (delayed-object)
  (funcall delayed-object))

(defun memo-proc (proc)
  (let ((already-run-p nil)
        (result nil))
    (lambda ()
      (if already-run-p
          result
          (prog1
              (setf result (funcall proc))
            (setf already-run-p t))))))

(defun make-message-stream (mailbox)
  (stream-cons
   (progn
     (let ((timeout (when *stop-time*
                      (- *stop-time* (get-internal-real-time)))))
       (multiple-value-bind (result success)
           (receive-message-with-timeout mailbox timeout)
         (if success
             result
             (error 'timeout-condition)))))
   (make-message-stream mailbox)))

(defun %stream-nth (n s)
  (if (= n 0)
      (%stream-car s)
      (%stream-nth (- n 1) (%stream-cdr s))))

(defun stream-nth (n s &optional (timeout *default-timeout*))
  (with-timeout timeout (%stream-nth n s)))

(defun %stream-find (item stream &key (test #'eql) (key #'identity))
  (declare (optimize space))
  (%stream-find-if (lambda (item2)
                     (funcall test item (funcall key item2)))
                   stream))

(defun stream-find (item stream &key (test #'eql) (key #'identity) (timeout *default-timeout*))
  (with-timeout timeout (%stream-find item stream :test test :key key)))

(defun %stream-find-if (predicate stream &key (key #'identity))
  (declare (optimize space))
  (if (funcall predicate (funcall key (%stream-car stream)))
      (%stream-car stream)
      (%stream-find-if predicate (%stream-cdr stream) :key key)))

(defun stream-find-if (predicate stream &key (key #'identity) (timeout *default-timeout*))
  (with-timeout timeout (%stream-find-if predicate stream :key key)))

(defun %stream-remove (item stream &key (test #'eql) (key #'identity) count)
  (declare (optimize space))
  (%stream-remove-if (lambda (item2)
                       (funcall test item (funcall key item2)))
                     stream
                     :count count))

(defun stream-remove (item stream &key (test #'eql) (key #'identity) count (timeout *default-timeout*))
  (with-timeout timeout (%stream-remove item stream :test test :key key :count count)))

(defun %stream-remove-if (predicate stream &key (key #'identity) count)
  (declare (optimize space))
  (check-type count (or null integer))
  (when (and count (<= count 0))
    (return-from %stream-remove-if stream))
  (if (funcall predicate (funcall key (%stream-car stream)))
      (if (and count (> count 1))
          (%stream-remove-if predicate (%stream-cdr stream) :key key :count (1- count))
          (%stream-cdr stream))
      (stream-cons (%stream-car stream)
                   (%stream-remove-if predicate (%stream-cdr stream) :key key :count count))))

(defun stream-remove-if (predicate stream &key (key #'identity) count (timeout *default-timeout*))
  (with-timeout timeout (%stream-remove-if predicate stream :key key :count count)))

(defmacro stream-dequeue (stream &optional (timeout *default-timeout*))
  `(with-timeout ,timeout
     (prog1 (%stream-car ,stream) (setf ,stream (%stream-cdr ,stream)))))
