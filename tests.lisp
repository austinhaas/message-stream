(in-package #:message-stream-tests)

(defun run-test (test-spec)
  "Same as run!, but adds an extra message."
  (format t "~&Testing: ~S" test-spec)
  (run! test-spec))

(defun make-cons-gen (&optional (char-repeat 26))
  "A simple generator that produces cons cells like (#\A . 0). The
letter and number will increment on each call, but the letter will
wrap after char-repeat generations."
  (let ((cur 0))
    (lambda ()
      (prog1
          (cons (code-char (+ (mod cur char-repeat) 65)) cur)
        (incf cur)))))

(defun make-test-stream (&key (char-repeat 26) (max-messages 26) (initial-delay 0) (msg-interval 100))
  "Returns a new message stream and spawns a thread to populate it a
regular intervals."
  (let* ((g (make-cons-gen char-repeat))
         (mailbox (make-mailbox :name "test-mailbox")))
    (sb-thread:make-thread (lambda ()
                             (sleep (/ initial-delay 1000))
                             (dotimes (i max-messages)
                               (send-message mailbox (funcall g))
                               (sleep (/ msg-interval 1000)))))
    (make-message-stream mailbox)))

(defun collect-stream (s n)
  (loop for i from 0 below n collect (stream-nth i s)))

(defun collect-gen (gen n)
  (loop repeat n collect (funcall gen)))

(def-suite basic-suite :description "Basic suite.")

(in-suite basic-suite)

(test timeout
  (let ((*default-timeout* 10)
        (s (make-message-stream (make-mailbox))))
    (signals (timeout-condition) (stream-car s))
    (signals (timeout-condition) (stream-cdr s))
    (signals (timeout-condition) (stream-nth 0 s))
    (signals (timeout-condition) (stream-find 0 s))
    (signals (timeout-condition) (stream-find-if #'constantly s))
    (signals (timeout-condition) (stream-remove 0 s))
    (signals (timeout-condition) (stream-remove-if #'constantly s))
    (signals (timeout-condition) (stream-dequeue s))))

(test basic-functions
  (let ((s (make-test-stream :char-repeat 5 :max-messages 8)))
    (is (equal '(#\A . 0) (stream-car s)))
    (is (equal '(#\B . 1) (stream-car (stream-cdr s))))
    (is (equal '(#\C . 2) (stream-nth 2 s)))
    (is (equal '(#\D . 3) (stream-find #\D s :test #'char= :key #'car)))
    (is (equal '(#\B . 1) (stream-find-if #'oddp s :key #'cdr)))
    (is (equal (collect-gen (make-cons-gen 5) 8)
               (collect-stream s 8)))))

(test dequeue
  (let ((s (make-test-stream :char-repeat 5 :max-messages 3)))
    (is (equal '(#\A . 0) (stream-dequeue s)))
    (is (equal '(#\B . 1) (stream-dequeue s)))
    (is (equal '(#\C . 2) (stream-dequeue s)))))

(test remove
  (let ((s (make-test-stream :char-repeat 5 :max-messages 10)))
    ;; Test that the items was removed and a new stream was created
    ;; without modifying the original stream.
    (let ((new-stream (stream-remove 2 s :key #'cdr :count 1)))
      (is (equal '(#\C . 2) (stream-nth 2 s)))
      (is (equal '(#\D . 3) (stream-nth 2 new-stream))))))

(defun run-tests ()
  (run-test 'basic-suite))
