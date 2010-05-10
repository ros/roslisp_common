(defpackage :test-data-struct
  (:use :cl :cl-test :priority-queue :cl-utils :extended-reals)
)

(in-package :test-data-struct)

(defvars q)

;; Priority queues
(setq q (make-priority-queue))

(check-equal (num-entries q) 0)
(check-error (dequeue-highest q))
(check-error (peek-highest q))

(enqueue q :foo 3)
(enqueue q :bar 'infinity)
(enqueue q :foo '-infinity)

(check-equal (num-entries q) 3)
(check-equal (peek-highest q) :bar)
(check-equal (dequeue-highest q) :bar)
(check-equal (num-entries q) 2)
(check-equal (peek-highest q) :foo)

(enqueue q :baz 4)

(check-equal (num-entries q) 3)
(check-equal (peek-highest q) :baz)
(check-equal (nth-value 1 (dequeue-highest q)) 4)