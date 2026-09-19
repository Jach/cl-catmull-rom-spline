(in-package #:cl-user)
(defpackage #:cl-catmull-rom-spline/test
  (:use #:common-lisp #:cl-catmull-rom-spline #:fiveam #:arrow-macros)
  (:local-nicknames (#:cr-spline #:cl-catmull-rom-spline)))
(in-package #:cl-catmull-rom-spline/test)
;(setf *run-test-when-defined* 'T)
;(setf fiveam:*debug-on-error* t)

(def-suite spline-test)
(in-suite spline-test)

(def-test sanity ()
  (let ((spline (->
                  (make-instance 'spline :dt 0.1)
                  (add-knot '(5 5))
                  (add-knot #(50 50))
                  (add-knot '(10 70)))))
    (loop for i from 1 to 25 do
          (multiple-value-bind (pt end seg-end) (next-point spline)
            (cond
              ((= i 1) (is-false end)
                       (is-false seg-end)
                       (is (equalp #(5.0 5.0) pt)))
              ((= i 11) (is-false end)
                        (is-true seg-end)
                        (is (and (= 50 (round (aref pt 0)))
                                 (= 50 (round (aref pt 1))))))
              ((= i 21) (is-false end)
                        (is-true seg-end)
                        (is (and (= 10 (round (aref pt 0)))
                                 (= 70 (round (aref pt 1))))))
              ((>= i 22) (is-false seg-end)
                         (is-true end)
                         (is (equalp #(0.0 0.0) pt)))
              (t (is-false end)
                 (is-false seg-end)))))
    (reset spline)
    (multiple-value-bind (pt end seg-end) (next-point spline)
      (is-false end)
      (is-false seg-end)
      (is (equalp #(5.0 5.0) pt)))))

(def-test can-handle-multiple-points ()
  (let ((spline (make-instance 'spline)))
    (loop for i from 0 to 20 do
          (add-knot spline (vector i 0)))
    (is (equalp #(0.0 0.0) (next-point spline)))))

(def-test scaling ()
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(1 2))
                  (add-knot '(2 4))
                  (add-knot '(3 6)))))
    (next-point spline) ; force computation of endpoints and coefficients
    (scale spline 2)
    (is (equalp #(#(2 4) #(4 8) #(6 12))
                (cr-spline::.knots spline)))
    (scale spline 0.5)
    (is (equalp #(#(1.0 2.0) #(2.0 4.0) #(3.0 6.0))
                (cr-spline::.knots spline)))))

(def-test endpoints-added-once ()
  "Check that computing the endpoints and coefficients once, invalidating them with a scale, and computing them again does not result in array size changes"
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(1 2))
                  (add-knot '(2 4))
                  (add-knot '(3 6)))))
    (next-point spline)
    (is-true (cr-spline::.endpoints-computed? spline))
    (is-true (cr-spline::.coeffs-computed? spline))
    (scale spline 2)
    (is-false (cr-spline::.endpoints-computed? spline))
    (is-false (cr-spline::.coeffs-computed? spline))
    (next-point spline)
    (is-true (cr-spline::.endpoints-computed? spline))
    (is-true (cr-spline::.coeffs-computed? spline))
    (is (equalp #(#(2 4) #(4 8) #(6 12))
                (cr-spline::.knots spline)))))

(def-test sufficient-knots ()
  (let ((spline (make-instance 'spline)))
    (signals error (cr-spline::compute-endpoints spline))
    (add-knot spline '(1 1))
    (signals error (cr-spline::compute-endpoints spline))
    (add-knot spline '(1 2))
    (signals error (cr-spline::compute-endpoints spline))
    (add-knot spline '(1 3))
    (finishes (cr-spline::compute-endpoints spline))))

(def-test alternate-alphas ()
  (let ((spline1 (->
                  (make-instance 'spline)
                  (add-knot '(1 2))
                  (add-knot '(2 4))
                  (add-knot '(3 6))))
        (spline2 (->
                   (make-instance 'spline :alpha 0.8)
                   (add-knot '(1 2))
                   (add-knot '(2 4))
                   (add-knot '(3 6)))))
    (next-point spline1)
    (next-point spline2)
    (let ((pt1 (next-point spline1))
          (pt2 (next-point spline2)))
      (is (not (equalp pt1 pt2))))))

(def-test points-3d ()
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(1 2 3))
                  (add-knot '(4 5 6))
                  (add-knot '(7 8 9)))))
    (finishes (next-point spline))))

(def-test inconsistent-sizes ()
  (let ((spline (make-instance 'spline)))
    (add-knot spline '(1 2))
    (signals error (add-knot spline '(1 2 3)))))

(def-test auto-close-spline ()
  (let ((spline1 (->
                   (make-instance 'spline)
                   (add-knot '(1 2))
                   (add-knot '(3 4))
                   (add-knot '(5 6))))
        (final-s1 nil)
        (spline2 (->
                   (make-instance 'spline :auto-close? t :dt 0.2)
                   (add-knot '(1 2))
                   (add-knot '(3 4))
                   (add-knot '(5 6))))
        (final-s2 nil))
    (loop for pt = (next-point spline1)
          until (equalp #(0.0 0.0) pt)
          do
          (setf final-s1 pt))
    (loop for pt = (next-point spline2)
          until (equalp #(0.0 0.0) pt)
          do
          (setf final-s2 pt))
    (is (equalp #(5.0 6.0) final-s1))
    (is (equalp #(1.0 2.0) final-s2))
    ))

(def-test knots-after-auto-close ()
  "Tests that a spline with auto close that started computing, then got interrupted and had more knots added to it, correctly alters the auto-closed end to always be the first knot."
  (let ((spline (->
                  (make-instance 'spline :auto-close? t)
                  (add-knot '(1 2))
                  (add-knot '(3 4))
                  (add-knot '(5 6)))))
    (next-point spline)
    (is (equalp #(#(1 2) #(3 4) #(5 6) #(1 2)) (cr-spline::.knots spline)))
    (add-knot spline '(7 8))
    (is (equalp #(#(1 2) #(3 4) #(5 6) #(7 8)) (cr-spline::.knots spline)))
    (next-point spline)
    (is (equalp #(#(1 2) #(3 4) #(5 6) #(7 8) #(1 2)) (cr-spline::.knots spline)))
    ))

(def-test endpoint-mode-reflect ()
  (let ((spline (make-instance 'spline :aux-endpoint-mode ':reflect)))
    (add-knot spline '(0 0))
    (add-knot spline '(10 0))
    (add-knot spline '(20 0))
    (compute-spline spline)
    ;; aux-start = reflect knots[1] about knots[0] = 2*(0,0) - (10,0) = (-10, 0)
    (is (equalp #(-10 0) (.aux-start spline)))
    ;; aux-end = reflect knots[1] about knots[2] = 2*(20,0) - (10,0) = (30, 0)
    (is (equalp #(30 0) (.aux-end spline)))))

(def-test endpoint-mode-explicit ()
  (let ((spline (make-instance 'spline :aux-endpoint-mode ':explicit)))
    (add-knot spline '(0 0))
    (add-knot spline '(10 0))
    (add-knot spline '(20 0))
    ;; should error before aux knots are set
    (signals error (compute-spline spline))
    (setf (.aux-start spline) #(-5 0)
          (.aux-end spline)   #(25 0))
    (finishes (compute-spline spline))))

(def-test global-point-at-odd-knots ()
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(0 0))
                  (add-knot '(10 5))
                  (add-knot '(20 0)))))
    (let ((start (global-point-at spline 0.0))
          (mid (global-point-at spline 0.5))
          (end   (global-point-at spline 1.0)))
      (is (= 0  (round (aref start 0))))
      (is (= 0  (round (aref start 1))))
      (is (= 10 (round (aref mid 0))))
      (is (= 5 (round (aref mid 1))))
      (is (= 20 (round (aref end 0))))
      (is (= 0  (round (aref end 1)))))))

(def-test global-point-at-even-knots ()
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(0 0))
                  (add-knot '(10 6))
                  (add-knot '(20 12))
                  (add-knot '(30 18)))))
    (let ((mid (global-point-at spline 0.5)))
      (is (equalp #(15.0 9.0) mid)))))

(def-test advance-by-distance-reaches-end ()
  "Advancing by the total arc length should land us at the last knot."
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(0 0))
                  (add-knot '(100 0))
                  (add-knot '(200 0)))))
    (compute-spline spline :compute-distance-table? t)
    (multiple-value-bind (pt done?) (cr-spline:advance-by-distance spline (cr-spline::.total-length spline))
      (is-true done?)
      (is (= 200 (round (aref pt 0))))
      (is (= 0   (round (aref pt 1)))))))

(def-test advance-by-distance-constant-speed ()
  "Advancing by equal distances should produce approximately equal world-space
   steps, unlike next-point which produces equal parameter steps."
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(0 0))
                  (add-knot '(0 100))   ; sharp turn
                  (add-knot '(100 100)))))
    (compute-spline spline :compute-distance-table? t)
    (let* ((step (/ (cr-spline::.total-length spline) 10))
           (prev (cr-spline:advance-by-distance spline step))
           (distances
             (loop repeat 9
                   for curr = (cr-spline:advance-by-distance spline step)
                   collect (cr-spline::euclidean-distance prev curr)
                   do (setf prev curr))))
      ;; All steps should be within ~5% of each other
      (let ((min-d (reduce #'min distances))
            (max-d (reduce #'max distances)))
        (is (< (/ (- max-d min-d) max-d) 0.05))))))

;; Helpers for the tests below

(defun vec~= (v1 v2 &optional (tolerance 1e-4))
  (every (lambda (a b) (< (abs (- a b)) tolerance)) v1 v2))

(defun segment-tangent (spline seg local-t)
  "The derivative p'(t) = 3a*t^2 + 2b*t + c of a segment, used to compare the direction of travel on either side of a knot."
  (let* ((coeffs (aref (cr-spline::.coeffs spline) seg))
         (as (aref coeffs 0))
         (bs (aref coeffs 1))
         (cs (aref coeffs 2)))
    (coerce (loop for d below (cr-spline::.pt-dimensions spline)
                  collect (+ (* 3 local-t local-t (aref as d))
                             (* 2 local-t (aref bs d))
                             (aref cs d)))
            'vector)))

(def-test traversal-ends-with-zero-vector ()
  "The sentinel is a zero vector matching the knot dimensions, and done? stays T on every later call."
  (let ((spline (->
                  (make-instance 'spline :dt 0.5)
                  (add-knot '(1 2 3))
                  (add-knot '(4 5 6))
                  (add-knot '(7 8 9))))
        (points '()))
    (loop
      (multiple-value-bind (pt done?) (next-point spline)
        (when done?
          (is (equalp #(0 0 0) pt))
          (return))
        (push pt points)))
    (setf points (nreverse points))
    (is (vec~= #(1.0 2.0 3.0) (first points)))
    (is (vec~= #(7.0 8.0 9.0) (car (last points))))
    ;; still done, still a zero vector
    (multiple-value-bind (pt done?) (next-point spline)
      (is-true done?)
      (is (equalp #(0 0 0) pt)))))

(def-test seg-end-flagged-once-per-segment ()
  "The local t is counted in whole dt steps rather than accumulated, so even small dt values land exactly on t=1."
  (dolist (dt '(0.5 0.2 0.1 0.04 0.025 0.01 0.005 0.001))
    (let ((spline (->
                    (make-instance 'spline :dt dt)
                    (add-knot '(0 0))
                    (add-knot '(10 10))
                    (add-knot '(20 0))
                    (add-knot '(30 10))))
          (seg-ends 0))
      (loop
        (multiple-value-bind (pt done? seg-end?) (next-point spline)
          (declare (ignore pt))
          (when done? (return))
          (when seg-end? (incf seg-ends))))
      (is (= 3 seg-ends) "dt ~a flagged ~d segment ends, expected 3" dt seg-ends))))

(def-test scale-with-auto-close ()
  "The auto-closed final knot is a copy of the first rather than the same object, so scaling can't apply to it twice."
  (let ((spline (->
                  (make-instance 'spline :auto-close? t)
                  (add-knot '(1 2))
                  (add-knot '(3 4))
                  (add-knot '(5 6)))))
    (next-point spline) ; appends the closing knot
    (scale spline 2)
    (is (equalp #(#(2 4) #(6 8) #(10 12) #(2 4)) (cr-spline::.knots spline)))))

(def-test coeffs-stay-in-step-with-knots ()
  "Adding a knot to an auto-closed spline pops the closing knot and re-appends it later; coeffs must not accumulate stale entries."
  (let ((spline (->
                  (make-instance 'spline :auto-close? t)
                  (add-knot '(1 2))
                  (add-knot '(3 4))
                  (add-knot '(5 6)))))
    (loop for i from 0 below 10 do
          (next-point spline)
          (add-knot spline (vector i i)))
    (next-point spline)
    (is (= (length (cr-spline::.knots spline))
           (length (cr-spline::.coeffs spline))))))

(def-test scale-with-explicit-endpoints ()
  "Explicit aux knots aren't recomputed from the knots, so scale has to scale them too or the end segments distort."
  (let ((spline (->
                  (make-instance 'spline :aux-endpoint-mode ':explicit)
                  (add-knot '(0 0))
                  (add-knot '(10 0))
                  (add-knot '(20 0)))))
    (setf (.aux-start spline) #(-10 0)
          (.aux-end spline) #(30 0))
    (compute-spline spline)
    (let ((before (cr-spline::point-in-segment spline 0 0.25)))
      (scale spline 2)
      (is (equalp #(-20 0) (.aux-start spline)))
      (is (equalp #(60 0) (.aux-end spline)))
      (compute-spline spline)
      (is (vec~= (map 'vector (lambda (v) (* 2 v)) before)
                 (cr-spline::point-in-segment spline 0 0.25))))))

(def-test scale-before-explicit-endpoints-set ()
  "Scaling shouldn't blow up merely because the explicit aux knots haven't been supplied yet; compute-endpoints is where that gets caught."
  (let ((spline (->
                  (make-instance 'spline :aux-endpoint-mode ':explicit)
                  (add-knot '(0 0))
                  (add-knot '(10 0))
                  (add-knot '(20 0)))))
    (finishes (scale spline 2))
    (is (equalp #(#(0 0) #(20 0) #(40 0)) (cr-spline::.knots spline)))
    (signals error (compute-spline spline))))

(def-test advance-by-distance-clamps-at-start ()
  "A negative distance shouldn't run off the front of the arc length table."
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(0 0))
                  (add-knot '(100 0))
                  (add-knot '(200 0)))))
    (advance-by-distance spline 50)
    (advance-by-distance spline -500)
    (is (= 0.0 (cr-spline::.arc-position spline)))
    (is (vec~= #(0.0 0.0) (advance-by-distance spline 0)))))

(def-test advance-by-distance-survives-scale ()
  "Arc length scales with the geometry, so a partly advanced spline stays at the same relative spot after a scale."
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(0 0))
                  (add-knot '(100 0))
                  (add-knot '(200 0)))))
    (advance-by-distance spline 100) ; halfway along a 200 unit path
    (scale spline 10)
    (multiple-value-bind (pt done?) (advance-by-distance spline 0)
      (is-false done?)
      (is (= 1000 (round (aref pt 0)))))))

(def-test closed-spline-wraps-aux-knots ()
  "A closed ring's neighbours are known rather than guessed at: aux-start is the knot before the seam, aux-end the one after."
  (let ((spline (->
                  (make-instance 'spline :auto-close? t)
                  (add-knot '(0 0))
                  (add-knot '(10 0))
                  (add-knot '(10 10))
                  (add-knot '(0 10)))))
    (compute-spline spline)
    (is (equalp #(0 10) (.aux-start spline)))
    (is (equalp #(10 0) (.aux-end spline)))
    ;; so the path arrives at the seam heading the same way it leaves it
    (let ((last-seg (- (length (cr-spline::.knots spline)) 2)))
      (is (vec~= (segment-tangent spline last-seg 1.0)
                 (segment-tangent spline 0 0.0))))))

(def-test closed-spline-ignores-endpoint-mode ()
  "Since the wrap-around is exact, :duplicate and :reflect describe the same closed curve, though they still differ while the spline is open."
  (flet ((path (&rest initargs)
           (let ((spline (apply #'make-instance 'spline :dt 0.25 initargs)))
             (dolist (knot '((0 0) (17 -3) (22 14) (6 21)))
               (add-knot spline knot))
             (loop for (pt done?) = (multiple-value-list (next-point spline))
                   until done? collect pt))))
    (is (equalp (path :auto-close? t :aux-endpoint-mode ':duplicate)
                (path :auto-close? t :aux-endpoint-mode ':reflect)))
    (is (not (equalp (path :aux-endpoint-mode ':duplicate)
                     (path :aux-endpoint-mode ':reflect))))))

(def-test explicit-endpoints-survive-auto-close ()
  "Auto-close defers to manually supplied aux knots rather than wrapping them."
  (let ((spline (->
                  (make-instance 'spline :auto-close? t :aux-endpoint-mode ':explicit)
                  (add-knot '(0 0))
                  (add-knot '(10 0))
                  (add-knot '(10 10)))))
    (signals error (compute-spline spline)) ; still required to set them
    (setf (.aux-start spline) #(-1 -1)
          (.aux-end spline) #(99 99))
    (compute-spline spline)
    (is (equalp #(-1 -1) (.aux-start spline)))
    (is (equalp #(99 99) (.aux-end spline)))))

(def-test advance-by-distance-on-degenerate-spline ()
  "A spline whose knots all coincide has zero arc length, so every sample in the table is equal and there is nothing to interpolate between."
  (let ((spline (->
                  (make-instance 'spline)
                  (add-knot '(5 5))
                  (add-knot '(5 5))
                  (add-knot '(5 5)))))
    (multiple-value-bind (pt done?) (advance-by-distance spline 10)
      (is-true done?)
      (is (equalp #(5.0 5.0) pt)))))

;; Purely for coverage satisfaction to get around SBCL not properly instrumenting inline functions.
;; The out-of-line copy of an inlined function is what the coverage report reads, and every ordinary
;; call site inlines instead of calling it, so the report shows those bodies as never reached.
;; Going through FDEFINITION is what actually reaches them -- a plain call, or even
;; (funcall #'name ...), still gets inlined and leaves the report unchanged.

(def-test knot-ref-out-of-line ()
  "KNOT-REF indexes the user knots, and reaches for the aux knots just outside them at -1 and at the knot count."
  (let ((spline (->
                  (make-instance 'spline :aux-endpoint-mode ':explicit)
                  (add-knot '(1 2))
                  (add-knot '(3 4))
                  (add-knot '(5 6)))))
    (setf (.aux-start spline) #(-10 -20)
          (.aux-end spline) #(70 80))
    (compute-spline spline)
    (let ((knot-ref (fdefinition 'cr-spline::knot-ref)))
      (is (= -10 (funcall knot-ref spline -1 0)))
      (is (= -20 (funcall knot-ref spline -1 1)))
      (is (= 1 (funcall knot-ref spline 0 0)))
      (is (= 6 (funcall knot-ref spline 2 1)))
      (is (= 70 (funcall knot-ref spline 3 0)))
      (is (= 80 (funcall knot-ref spline 3 1))))))

(def-test dot-product-out-of-line ()
  (is (= 32 (funcall (fdefinition 'cr-spline::dot-product) #(1 2 3) #(4 5 6)))))

(def-test euclidean-distance-out-of-line ()
  (let ((distance (fdefinition 'cr-spline::euclidean-distance)))
    (is (= 5 (funcall distance #(0 0) #(3 4))))
    (is (= 7 (funcall distance #(0 0 0) #(2 3 6))))))
