(in-package #:cl-user)
(defpackage #:cl-catmull-rom-spline
  (:nicknames #:cm-spline #:com.thejach.cl-catmull-rom-spline)
  (:use #:common-lisp)

  ;; class
  (:export
    #:spline)

  ;; generic functions
  (:export
    #:add-knot
    #:next-point
    #:reset
    #:scale
    ))

(in-package #:cm-spline)

(defclass spline ()
  ((knots :accessor knots :initform (make-array 1 :fill-pointer 0 :adjustable t))
   (coeffs :accessor coeffs :initform (make-array 1 :fill-pointer 0 :adjustable t))

   (knots-count :accessor knots-count :initform 0)
   (current-knot :accessor cur-knot :initform 1)
   (current-t :accessor cur-t :initform 0.0)

   (delta-t :accessor dt :initarg :dt :initform 0.1
            :documentation
            "By default this 0.1 dt means that for the first
             segment, it will take 11 calls (t goes from =0 to =1)
             to complete, but every other segment will start at t=0.1
             to avoid repeating the knot value at t=0 and thus take
             10 calls to go through. Use either shorter point distances,
             or a lower dt to have smoother interpolation.")
   (endpoints-computed? :accessor endpoints-computed? :initform nil)
   (coeffs-computed? :accessor coeffs-computed? :initform nil))

  (:documentation
    "Catmull-Rom splines are composed of a number of knots
     (at least 3) with a piecewise path computed between
     each knot, with the overall path from the first to
     last knot appearing smooth. Each knot will be crossed
     along the path.

     Point: two-element vector representing an (x, y) coordiante.
     Knots: control points determining the shape of the complete spline path.
     Spline segments: the path between knots. Can be thought of as functions of time
       path(t) : R -> point, from t=0 to t=1.
       path(0) is the first knot in the segment, path(1) is the last knot."))

(defgeneric add-knot (spline knot)
  (:documentation
    "Given a knot as a sequence pair representing an x,y point,
     add it to this spline's control knots."))

(defgeneric next-point (spline)
  (:documentation
    "For the current segment between knots, calculates the next point and
     returns it as a two element vector.

     If the spline contains fewer than 3 knots, an error signal is raised.

     This method returns two extra multiple values for special conditions:
     * When the next point has reached or would reach beyond the final knot,
       #(0.0 0.0) is returned along with a second value set to T.
       Otherwise this second value is nil.
     * When the returned point is the final point of a segment, a third value will be T."))

(defgeneric reset (spline)
  (:documentation
    "Reinitializes the spline state so the next call to next-poin will return the beginning point of the spline."))

(defgeneric scale (spline factor)
  (:documentation
    "Multiplies each knot's x and y values by the scale factor."))


(defmethod add-knot ((spline spline) (knot sequence))
  (when (zerop (knots-count spline)) ; first knot should be at idx 1 to leave room for auto-computed knot endpoints
    (vector-push (vector 0.0 0.0) (knots spline)))

  (vector-push-extend (vector (elt knot 0) (elt knot 1))
                      (knots spline))
  (incf (knots-count spline))
  ; keep coeffs vector size the same, will be properly set when computing coeffs:
  (vector-push-extend (vector 0.0 0.0) (coeffs spline))
  spline)

(defmacro knot-i-x (spline i)
  `(aref (aref (knots ,spline) ,i) 0))

(defmacro knot-i-y (spline i)
  `(aref (aref (knots ,spline) ,i) 1))

(defmethod scale ((spline spline) (factor real))
  (loop for i from 1 to (knots-count spline) do
        (setf (knot-i-x spline i) (* factor (knot-i-x spline i))
              (knot-i-y spline i) (* factor (knot-i-y spline i))))
  (compute-endpoints spline)
  (compute-coefficients spline)
  spline)

(defmethod reset ((spline spline))
  (setf (cur-knot spline) 1
        (cur-t spline) 0.0))

(defmethod next-point ((spline spline))
  (when (not (endpoints-computed? spline))
    (compute-endpoints spline))
  (when (not (coeffs-computed? spline))
    (compute-coefficients spline))

  (when (>= (cur-t spline) 1.002) ; end of segment + 1 (that's why not exactly 1)
    (incf (cur-knot spline))
    (setf (cur-t spline) (dt spline))) ; skip repeating the knot value (if not set to 0)

  (when (>= (cur-knot spline) (knots-count spline))
    (return-from next-point (values #(0.0 0.0) T NIL)))

  ; from previously computed coefficients a,b,c,d for each dimension X and Y, the points
  ; x, y = a*t^3 + b*t^2 + c*t + d
  (let* ((tt (cur-t spline))
         (t-vec (vector (* tt tt tt) (* tt tt) tt 1))
         (x (dot-product (vector (coeff-x spline 'a) (coeff-x spline 'b) (coeff-x spline 'c) (coeff-x spline 'd))
                         t-vec))
         (y (dot-product (vector (coeff-y spline 'a) (coeff-y spline 'b) (coeff-y spline 'c) (coeff-y spline 'd))
                         t-vec)))
    (incf (cur-t spline) (dt spline))
    (values (vector x y) nil (>= tt 1))))

(defun coeff->idx (coeff)
  (getf '(a 0 b 1 c 2 d 3) coeff))

(defun coeff-x (spline coeff)
  (aref
    (aref (aref (coeffs spline) (cur-knot spline))
          (coeff->idx coeff))
    0))

(defun coeff-y (spline coeff)
  (aref
    (aref (aref (coeffs spline) (cur-knot spline))
          (coeff->idx coeff))
    1))

(defun dot-product (m1 m2)
  (loop for v1 across m1
        for v2 across m2
        summing (* v1 v2)))

(defmethod compute-endpoints ((spline spline))
  "Should be called after all knots have been added. Called automatically by next-point if user forgot.

   Once the user-facing knots are added, we need to compute two 'true' auxilliary endpoints for the whole path.
   Currently this is done by duplicating the the first and last knots, though this can cause kinking.
   A future approach is to use reflection, which can still cause pinching but may be better.
   Of course, they can be set manually if needed. (Consider subclassing rather than direct knot access.)"
  (let ((knots (knots spline))
        (count (knots-count spline)))
    (unless (>= count 3)
      (error "Invalid state. You need to have at least 3 knots added to use the spline."))

    (setf (aref knots 0) ; left endpoint duplicates first knot (idx 1)
          (aref knots 1))

    (let ((right-endpoint (aref knots count)))
      (if (>= (1+ count) (array-dimension knots 0))
          (vector-push-extend right-endpoint
                              knots)
          (setf (aref knots (1+ count))
                right-endpoint)))
    (setf (endpoints-computed? spline) T)))

(defun compute-coefficients (spline)
  "A point on a segment can be found by applying the equation
   p(t) = at^3 + bt^2 + ct + d
   with coeffs a, b, c, d. The coeffs are computed below,
   with e.g. a_x = 1/2(-x_{i-1} + 3x_i - 3_x{i+1} + x_{i+2})

   Note the basis matrix for cardinal spline coeffs is:
   [[-a 2-a a-2 a]
    [2a a-3 3-2a -a]
    [-a 0 a 0]
    [0 1 0 0]]
   where a is the spline 'tension'. As a approaches 1, the bend at each knot is less.
   Catmull-Rom splines typically use the value of 0.5, we use the same."
  (let ((mat #(#(-0.5 1.5 -1.5 0.5)
               #(1.0 -2.5 2.0 -0.5)
               #(-0.5 0.0 0.5 0.0)
               #(0.0 1.0 0.0 0.0))))
    (loop for i from 1 below (knots-count spline) do
          (let* ((x_{i-1} (knot-i-x spline (1- i)))
                 (x_i (knot-i-x spline i))
                 (x_{i+1} (knot-i-x spline (1+ i)))
                 (x_{i+2} (knot-i-x spline (+ 2 i)))

                 (y_{i-1} (knot-i-y spline (1- i)))
                 (y_i (knot-i-y spline i))
                 (y_{i+1} (knot-i-y spline (1+ i)))
                 (y_{i+2} (knot-i-y spline (+ 2 i)))

                 (ax (dot-product (aref mat 0) (vector x_{i-1} x_i x_{i+1} x_{i+2})))
                 (ay (dot-product (aref mat 0) (vector y_{i-1} y_i y_{i+1} y_{i+2})))
                 (bx (dot-product (aref mat 1) (vector x_{i-1} x_i x_{i+1} x_{i+2})))
                 (by (dot-product (aref mat 1) (vector y_{i-1} y_i y_{i+1} y_{i+2})))
                 (cx (dot-product (aref mat 2) (vector x_{i-1} 0.0 x_{i+1} 0.0)))
                 (cy (dot-product (aref mat 2) (vector y_{i-1} 0.0 y_{i+1} 0.0)))
                 (dx (dot-product (aref mat 3) (vector 0.0 x_i 0.0 0.0)))
                 (dy (dot-product (aref mat 3) (vector 0.0 y_i 0.0 0.0))))
            (set-coeffs spline i ax ay bx by cx cy dx dy))))
  (setf (coeffs-computed? spline) t))

(defun set-coeffs (spline i ax ay bx by cx cy dx dy)
  (setf (aref (coeffs spline) i)
        (vector
          (vector ax ay)
          (vector bx by)
          (vector cx cy)
          (vector dx dy))))

