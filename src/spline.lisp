(in-package #:cl-user)
(defpackage #:cl-catmull-rom-spline
  (:use #:common-lisp)

  ;; class and two optional fields
  (:export
    #:spline
    #:.aux-start
    #:.aux-end)

  ;; generic functions
  (:export
    #:add-knot
    #:next-point
    #:global-point-at
    #:advance-by-distance
    #:reset
    #:scale
    #:compute-spline
    ))

(in-package #:cl-catmull-rom-spline)

(defclass spline ()
  ((knots :accessor .knots :initform (make-array 3 :fill-pointer 0 :adjustable t))
   (coeffs :accessor .coeffs :initform (make-array 3 :fill-pointer 0 :adjustable t))

   (aux-endpoint-mode :accessor .aux-endpoint-mode :initarg :aux-endpoint-mode :initform ':duplicate
                      :documentation
                      "One of :duplicate (default), :reflect, or :explicit. Controls the auxiliary endpoints before the start and end of the spline path itself, which
                       slightly affect the behavior of the spline. Duplicating can cause kinks, reflecting avoids kinks but can cause pinches.
                       If :explicit, .AUX-START and .AUX-END *must* be set manually.
                       Note that if :auto-close? is set to T, then this mode does nothing for the :duplicate and :reflect cases, as we do a proper wrap-around for the aux endpoints.")
   (aux-start :accessor .aux-start :initform nil :documentation "The auxiliary knot that must be at the start of a spline. Not used on the path itself.")
   (aux-end :accessor .aux-end :initform nil :documentation "The auxiliar knot that must be at the end of the spline. Not used on the path itself.")

   (point-dimensions :accessor .pt-dimensions :initform 0 :documentation "Number of coordinates per knot/point along the spline. (2 for 2D, 3 for 3D). Inferred from the first knot added.")

   (current-knot :accessor .cur-knot :initform 0)
   (current-step :accessor .cur-step :initform 0 :documentation "Which DELTA-T step within the current segment that NEXT-POINT will return next.")
   (current-t :accessor .cur-t :initform 0.0 :documentation "A local piecewise time-value t that ranges from 0-1 between each pair of knots, derived from CURRENT-STEP.")

   (delta-t :accessor .dt :initarg :dt :initform 0.1
            :documentation
            "The step value used to traverse the spline per call to NEXT-POINT within a segment.
             The first segment takes ceil(1/dt)+1 calls (t from 0 to 1 inclusive), or 11 by default,
             subsequent segments take ceil(1/dt) calls (t starts at dt to avoid repeating the shared knot at t=0), or 10 by default.
             Use either shorter point distances, or a lower dt to have smoother interpolation.")

   (alpha :accessor .alpha :initarg :alpha :initform 0.5
          :documentation
          "The cardinal spline basis tension parameter. The default 0.5 is the standard Catmull-Rom spline value. As it approaches 0, however,
           it's as if the spline is a rope that is being tightened taught through the knots, and each segment collapses onto a straight chord between knots.")

   (auto-close? :accessor .auto-close? :initarg :auto-close? :initform nil
                :documentation
                "When T, a final knot equal to the first knot given with ADD-KNOT is automatically appended when computing endpoints, thus closing the curve into a loop.
                 Auxiliary endpoints are also set automatically (unless the spline is in :explicit mode) to create a proper wrap-around for the loop.")

   (arc-length-tables :accessor .arc-length-tables :initform nil :documentation "Tables of arc length info per knot segment, used when using ADVANCE-BY-DISTANCE.")
   (arc-position :accessor .arc-position :initform 0.0 :documentation "Tracks how far advanced along the spline.")
   (total-length :accessor .total-length :initform 0.0 :documentation "Total arc length of the spline, used when using ADVANCE-BY-DISTANCE.")

   (did-auto-close? :accessor .did-auto-close? :initform nil)
   (endpoints-computed? :accessor .endpoints-computed? :initform nil)
   (coeffs-computed? :accessor .coeffs-computed? :initform nil))

  (:documentation
    "Catmull-Rom splines are composed of a number of knots
     (at least 3) with a piecewise path computed between
     each knot, with the overall path from the first to
     last knot appearing smooth. Each knot will be crossed
     along the path.

     Typical usage is to create a spline object, call ADD-KNOT to add knots and define a path, and then traverse interpolated points along the path by repeatedly calling NEXT-POINT.

     Knots are sequences of N real numbers representing coordinates. Typical use is 2D or 3D, e.g. #(x y) or #(x y z), but any N >= 1 works.
     Knots are used as control points determining the shape of the complete spline path.

     Points are N-element vectors along the path. #(x y) would represent a 2D coordinate, #(x y z) would represent a 3D coordinate.

     Spline segments refer to the path between two knots. They can be thought of as functions of local time:
       path(t) : R -> point, from t=0 to t=1.
       path(0) is the first knot in the segment, path(1) is the last knot."))

;; Spline API

(defgeneric add-knot (spline knot)
  (:documentation
    "Given a knot as a sequence of real numbers representing an N-dimensional point,
     add it to this spline's control knots. All knots must have the same dimension which is fixed by the first knot added."))

(defgeneric next-point (spline)
  (:documentation
    "Advances along the spline path by delta-t and calculates the new current point as a vector of length N, the same dimensions as the knots.
     Returns the following 3 multiple-values:
     * next-point - this new current coordinate vector calculated by this call, or a zero-vector if called again after the final knot's coordinates were returned.
     * done? - T when the end of the spline (the final knot) has been reached. Note that this is T only in calls after the one that returned the final knot's coordinates.
     * seg-end? - T when this returned point is the final point of a segment, i.e. we are exactly on a knot.

     Therefore to advance along the entire spline with the first point returned being the same as the first knot, and the last point returned being the same as the last knot,
     repeatedly call NEXT-POINT until either the returned point is a zero-vector (like #(0 0) for 2D), or until the second value is T (useful if there is ambiguity from #(0 0) being a legitimate point along the path).

     If the spline contains fewer than 3 knots, an error signal is raised.
     "))

(defgeneric global-point-at (spline time)
  (:documentation
    "Treating the entire spline as a curve path with start at time=0 and end at time=1, returns the point as a vector at the specified TIME.
     A uniform weighting is assumed by default -- thus a value of 0.5 should return the halfway point of the segment that is in the middle of the spline,
     for even numbers of knots, or the middle knot itself for odd numbers of knots.
     However if knots are unevenly spaced, the point may not be very similar to the geometric middle of the curve."))

(defgeneric reset (spline)
  (:documentation
    "Reinitializes the spline state so it can be traversed from the start again. i.e. the next call to NEXT-POINT will return the beginning of the spline."))

(defgeneric scale (spline factor)
  (:documentation
    "Multiplies each knot's coordinate values by the scale factor.
     Invalidates any previously computed coefficients."))

(defgeneric compute-spline (spline &key compute-distance-table?)
  (:documentation
    "Meant to be called after knots have been loaded, performs the computations needed for the rest of the spline, which can be expensive, without delaying it to the first call of NEXT-POINT.
     The optional keyword COMPUTE-DISTANCE-TABLE? is further used to control whether the distance table needed by ADVANCE-BY-DISTANCE is created ahead of time."))

(defgeneric advance-by-distance (spline distance)
  (:documentation
    "An alternative way to traverse the spline than by calling NEXT-POINT. Given a world-space distance, move along the spline by that amount and return the current point. Will not extrapolate backwards from the spline's start point.
     Returns a second value done? indicating that the end of the spline has been reached."))

;; Implementation

(defmethod add-knot ((spline spline) (knot sequence))
  (let ((dims (length knot)))
    (when (zerop (length (.knots spline)))
      (setf (.pt-dimensions spline) dims))

    (assert (= dims (.pt-dimensions spline)) ()
            "Knot has ~d dimension~:p but spline expects ~d." dims (.pt-dimensions spline))

     (when (.did-auto-close? spline)
        (vector-pop (.knots spline)) ; undo auto-close duplicate, will be added again to the end later
        (vector-pop (.coeffs spline))
        (setf (.did-auto-close? spline) nil))

    (%add-knot spline (copy-knot knot))
    spline))

(defun zero-vec (n)
  (make-array n :initial-element 0))

(defun copy-knot (knot)
  "Ensures the given knot sequence is a simple-vector."
  (map 'simple-vector #'identity knot))

(defun %add-knot (spline knot)
  "Adds the knot to the internal knots vector and updates other spline state."
  (vector-push-extend knot (.knots spline))
  ;; Keep coeffs vector size the same, elements will be properly set when computing coeffs:
  (vector-push-extend (zero-vec (.pt-dimensions spline)) (.coeffs spline))
  (invalidate-computed-state spline))


(declaim (inline knot-ref))
(defun knot-ref (spline knot-index dim-index)
  "Returns the specified dimension of the specified knot. Outside the user-added control knots, the auxiliary knots are used."
  (let ((knot (cond
                ((eql knot-index -1) (.aux-start spline))
                ((eql knot-index (length (.knots spline))) (.aux-end spline))
                (t (aref (.knots spline) knot-index)))))
    (aref knot dim-index)))

(declaim (inline dot-product))
(defun dot-product (v1 v2)
  (loop for el1 across v1
        for el2 across v2
        summing (* el1 el2)))

(defmethod scale ((spline spline) (factor real))
  (let ((knots (if (and (eql (.aux-endpoint-mode spline) :explicit) ; aux knots get auto recomputed in other cases, but here we need to scale the user-supplied ones (if already supplied) so that the ends aren't distorted
                        (.aux-start spline) (.aux-end spline))
                   (concatenate 'vector (.knots spline) (list (.aux-start spline) (.aux-end spline)))
                   (.knots spline))))
    (loop for knot across knots do
          (loop for d below (.pt-dimensions spline) do
                (setf (aref knot d) (* factor (aref knot d))))))
  (setf (.arc-position spline) (* (abs factor) (.arc-position spline))) ; keeps any in-progress advance-by-distance the same relative spot
  (invalidate-computed-state spline)
  spline)


(defmethod reset ((spline spline))
  (setf (.cur-knot spline) 0
        (.cur-step spline) 0
        (.cur-t spline) 0.0
        (.arc-position spline) 0.0))

(defun invalidate-computed-state (spline)
  (setf (.endpoints-computed? spline) nil
        (.coeffs-computed? spline) nil
        (.total-length spline) 0.0))

(defmethod compute-spline ((spline spline) &key compute-distance-table?)
  (when (not (.endpoints-computed? spline))
    (compute-endpoints spline))
  (when (not (.coeffs-computed? spline))
    (compute-coefficients spline))
  (when (and (zerop (.total-length spline)) compute-distance-table?)
    (build-arc-length-table spline)))


(defmethod next-point ((spline spline))
  (compute-spline spline)

  (let ((dt (.dt spline)))
    (when (> (* (.cur-step spline) dt) (1+ (* 0.001 dt))) ; stepped past t=1 for the current segment, so advance to next segment
      (incf (.cur-knot spline))
      (setf (.cur-step spline) 1)) ; skip t=0 to avoid repeating the shared knot value

    (when (>= (.cur-knot spline) (1- (length (.knots spline)))) ; ran out of segments, we're done
      (return-from next-point (values (zero-vec (.pt-dimensions spline)) T NIL)))

    (let* ((local-t (* (.cur-step spline) dt))
           (point (point-in-segment spline (.cur-knot spline) local-t))
           (seg-done? (>= local-t 1.0)))
      (setf (.cur-t spline) local-t) ; set so we count in whole delta-t increments rather than inaccurate accumulations for certain dt values
      (incf (.cur-step spline))
      (values point nil seg-done?))))

(defun point-in-segment (spline knot-id local-t)
  "Computes the point as a coordinate vector along the spline segment that starts with knot-id, with a local-t from [0, 1].
   From previously computed a,b,c,d coefficients on this knot's segment, for each spatial dimension, the point is
   p(t) = a*t^3 + b*t^2 + c*t + d."
  (let* ((tt local-t)
         (t2 (* tt tt))
         (t3 (* tt tt tt))
         (coeffs (aref (.coeffs spline) knot-id))
         (as (aref coeffs 0))
         (bs (aref coeffs 1))
         (cs (aref coeffs 2))
         (ds (aref coeffs 3))
         (result (zero-vec (.pt-dimensions spline))))
    (loop for d below (.pt-dimensions spline) do
          (setf (aref result d)
                (+ (* t3 (aref as d))
                   (* t2 (aref bs d))
                   (* tt (aref cs d))
                   (aref ds d))))
    result))


(defmethod compute-endpoints ((spline spline))
  "Once the user-facing knots are added, we need to compute two auxiliary endpoints at each end of the whole spline path.
   By default this is done by duplicating the first and last user-added knots, though this can cause kinking. (Or stalling.)
   Another option is with reflection, which can still cause pinching but avoids kinking.
   A third option is to be explicit and set them manually."
  (let ((knots (.knots spline)))
    (unless (>= (length knots) 3)
      (error "Invalid state. You need to have at least 3 knots added to use the spline. (Currently at ~d.)" (length knots)))

    ;; If auto-close? is set, append a copy of the first user knot so the curve loops
    (when (and (.auto-close? spline) (not (.did-auto-close? spline)))
      (%add-knot spline (copy-knot (aref knots 0)))
      (setf knots (.knots spline))
      (setf (.did-auto-close? spline) t))

    ;; Also if auto-close? was set and applied, then unless the aux knots are manually supplied, we have a correct wrap-around solution for the closed spline ring
    ;; rather than having to make a best-guess with duplication or reflection. (knots[n-2] and knots[1].)
    (when (and (.did-auto-close? spline) (not (eql (.aux-endpoint-mode spline) :explicit)))
      (setf (.aux-start spline) (aref knots (- (length knots) 2))
            (.aux-end spline) (aref knots 1)
            (.endpoints-computed? spline) T)
      (return-from compute-endpoints T))

    (case (.aux-endpoint-mode spline)
      (:duplicate
        (setf (.aux-start spline) (aref knots 0))
        (setf (.aux-end spline) (aref knots (1- (length knots)))))
      (:reflect
        (setf (.aux-start spline) (reflect-point (aref knots 0) (aref knots 1))) ; reflect P_1 about P_0
        (setf (.aux-end spline) (reflect-point (aref knots (1- (length knots))) (aref knots (1- (1- (length knots)))))) ; reflect P_{n-2} about P_{n-1}
        )
      (:explicit
        (assert (and (.aux-start spline) (.aux-end spline)) ()
                "Endpoint mode was set as :explicit but both AUX-START and AUX-END need to be set manually.")))

    (setf (.endpoints-computed? spline) T)))

(defun reflect-point (pivot source)
  "Returns the reflection of vector SOURCE about vector PIVOT.
   i.e. reflection = pivot + (pivot - source) = 2*pivot - source."
  (map 'simple-vector (lambda (p s) (- (* 2 p) s)) pivot source))


(defun compute-coefficients (spline)
  "For each spline segment, the coeffs are computed below,
   by multiplying the basis matrix with the four relevant neighboring control point coordinates [p_{i-1}, p_i, p_{i+1}, p_{i+2}] across each dimension.
   e.g. a_x = 1/2(-x_{i-1} + 3x_i - 3_x{i+1} + x_{i+2})
   "
  (let ((mat (basis-matrix (.alpha spline)))
        (dims (.pt-dimensions spline)))
    (loop for knot-id from 0 below (1- (length (.knots spline))) do
          (let ((as (zero-vec dims))
                (bs (zero-vec dims))
                (cs (zero-vec dims))
                (ds (zero-vec dims)))
            (loop for d below dims do
                  (let* ((p_{i-1} (knot-ref spline (1- knot-id) d))
                         (p_i (knot-ref spline knot-id d))
                         (p_{i+1} (knot-ref spline (1+ knot-id) d))
                         (p_{i+2} (knot-ref spline (+ 2 knot-id) d))

                         (pt-vector (vector p_{i-1} p_i p_{i+1} p_{i+2})))
                    (setf (aref as d) (dot-product (aref mat 0) pt-vector)
                          (aref bs d) (dot-product (aref mat 1) pt-vector)
                          (aref cs d) (dot-product (aref mat 2) pt-vector)
                          (aref ds d) (dot-product (aref mat 3) pt-vector))))
            (setf (aref (.coeffs spline) knot-id) (vector as bs cs ds)))))
  (setf (.coeffs-computed? spline) T))

(defun basis-matrix (alpha)
  "Given a tension value alpha, return the 4x4 cardinal spline basis matrix.
   An alpha of 0.5 gives the standard Catmull-Rom basis.
   The multiplication of the column vector
   ;[P_{i-1}; P_i; P_{i+1}; P_{i+2}] with this matrix gives coefficients [a; b; c; d] such that
   P(t) = a*t^3 + b*t^2 + c*t + d."
  (vector
    (vector (- alpha)   (- 2 alpha) (- alpha 2)       alpha)
    (vector (* 2 alpha) (- alpha 3) (- 3 (* 2 alpha)) (- alpha))
    (vector (- alpha)   0.0         alpha             0.0)
    (vector 0.0         1.0         0.0               0.0)))

#+nil
(let ((mat #(#(-0.5 1.5 -1.5 0.5)
             #(1.0 -2.5 2.0 -0.5)
             #(-0.5 0.0 0.5 0.0)
             #(0.0 1.0 0.0 0.0))))
  (equalp mat (basis-matrix 0.5)))


(defmethod global-point-at ((spline spline) time)
  "time = 0 represents the first knot, time = 1 represents the last knot."
  (compute-spline spline)
  (let* ((segs (1- (length (.knots spline))))
         (scaled (* (max 0.0 (min 1.0 time)) segs))
         (knot-idx (min (floor scaled) (1- segs)))
         (local-t (- scaled knot-idx)))
    (point-in-segment spline knot-idx local-t)))


(defmethod advance-by-distance ((spline spline) distance)
  (compute-spline spline :compute-distance-table? t)
  (setf (.arc-position spline) (max 0.0 (+ (.arc-position spline) distance)))
  (let* ((clamped (min (.arc-position spline) (.total-length spline)))
         (done? (>= (.arc-position spline) (.total-length spline))))
    (values (point-at-arc-length spline clamped) done?)))

(defstruct arc-length-table
  "Stores precomputed arc-length distance samples for a single spline segment.
   Each length in LENGTHS is the cumulative arc length at that sample.
   Each time in LOCAL-TS is the corresponding local time in the segment (0 to 1) at that sample.
   The TOTAL-LENGTH is the total arc length of this segment (and will equal the last entry in LENGTHS)."
  (lengths nil :type simple-vector)
  (local-ts nil :type simple-vector)
  (total-length 0.0))

(declaim (inline euclidean-distance))
(defun euclidean-distance (p1 p2)
  (sqrt (loop for i below (length p2)
              sum (expt (- (aref p2 i) (aref p1 i)) 2))))

(defun build-arc-length-table (spline &optional (samples-per-segment 100))
  "Walks each segment with SAMPLES-PER-SEGMENT steps, recording cumulative arc length distance at each sample. Stores an arc-length-table struct per segment."
  (let* ((segs (1- (length (.knots spline))))
         (seg-tables (make-array segs))
         (step-dt (/ 1.0 samples-per-segment)))
    (loop for seg below segs do
          (let ((lengths (make-array (1+ samples-per-segment)))
                (local-ts (make-array (1+ samples-per-segment)))
                (cumulative 0.0)
                (prev-point (point-in-segment spline seg 0.0)))
            (setf (aref lengths 0) 0.0
                  (aref local-ts 0) 0.0)
            (loop for step from 1 to samples-per-segment do
                  (let* ((local-t (* step step-dt))
                         (cur-point (point-in-segment spline seg local-t))
                         (dist (euclidean-distance prev-point cur-point)))
                    (incf cumulative dist)
                    (setf (aref lengths step) cumulative
                          (aref local-ts step) local-t)
                    (setf prev-point cur-point)))
            (setf (aref seg-tables seg) (make-arc-length-table :lengths lengths :local-ts local-ts :total-length cumulative))))
    (setf (.arc-length-tables spline) seg-tables
          (.total-length spline) (loop for tbl across seg-tables sum (arc-length-table-total-length tbl)))))

(defun point-at-arc-length (spline arc-pos)
  "Returns the point at the absolute arc-pos along the spline."
  (let ((remaining arc-pos)
        (length-tables (.arc-length-tables spline)))
    (loop for seg below (length length-tables)
          for table = (aref length-tables seg)
          do
          (let ((seg-len (arc-length-table-total-length table)))
            (when (or (<= remaining seg-len)
                      (= seg (1- (length length-tables))))
              (return (point-in-segment spline seg (point-at-seg-arc-length table remaining))))
            (decf remaining seg-len)))))

(defun point-at-seg-arc-length (table arc-pos)
  "Binary search within table to find the two samples bracketing the arc-pos, then linearly interpolate them for the corresponding local-t.
   Returns a local-t that can be used to evaluate the point along the segment at that t."
  (let* ((lengths (arc-length-table-lengths table))
         (local-ts (arc-length-table-local-ts table))
         (lo 0)
         (hi (1- (length lengths))))
    (loop while (> (- hi lo) 1) do
          (let ((mid (floor (+ lo hi) 2)))
            (if (<= (aref lengths mid) arc-pos)
                (setf lo mid)
                (setf hi mid))))

    (let ((arc0 (aref lengths lo))
          (arc1 (aref lengths hi))
          (t0 (aref local-ts lo))
          (t1 (aref local-ts hi)))
      (if (= arc0 arc1)
          t0
          (+ t0 (* (- t1 t0) (/ (- arc-pos arc0) (- arc1 arc0)))))
      )))

