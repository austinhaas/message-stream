(in-package #:message-stream)

(defparameter *default-timeout* 5
  "The default amount of time, in seconds, that an operation will run
before signalling a timeout-condition.")

(define-condition timeout-condition (condition)
  ()
  (:documentation "Signalled when an operation times out."))

(defparameter *stop-time* nil
  "This variable is bound automatically via the with-timeout macro.")

(defmacro with-timeout (timeout exp)
  `(let ((*stop-time* (when ,timeout (+ (get-internal-real-time) (* ,timeout internal-time-units-per-second)))))
     ,exp))

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
           (sb-concurrency:receive-message mailbox :timeout (when timeout (/ timeout internal-time-units-per-second)))
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
