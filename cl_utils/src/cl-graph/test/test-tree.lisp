(in-package :test-graph)



(check-true (not (is-tree g)))

(let* ((links '((f (b (:length . 2)))
		(b (a (:length . 3)))
		(a (c (:length . 3.2)) (e (:length . 2.4)))
		(c)
		(e)
		(d (f (:length . 1.6)))))

       (g (make-undirected-graph links)))

  (check-true (is-tree g)))

(let* ((links '((f (b (:length . 2)))
		(b (a (:length . 3)))
		(a (c (:length . 3.2)) (e (:length . 2.4)))
		(c (d (:length . 4.1)))
		(d (f (:length . 1.6)))
		(e)
		(g (h (:length . 4.4)) (i (:length . 2.1)))
		(h (i (:length . 1.6)) (j (:length . 3.3)))
		(i)
		(j)))
       (g (make-undirected-graph links)))
  (check-true (not (is-tree g))))


