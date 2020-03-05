(in-package #:cl-user)
(defpackage #:cl-catmull-rom-spline/example
  (:nicknames #:spline-example)
  (:use #:common-lisp #:cm-spline)
  (:export #:launch #:main))
(in-package #:cl-catmull-rom-spline/example)

(defvar *knots* (list)
  "List of knot xy vector pairs to use for the spline")

(defvar *hide-help?* nil
  "Toggles rendering of help text")

(defvar *hide-knots?* nil
  "Toggles rendering of added knots")

(defvar *sprite* nil
  "Object surface to animate over the spline")

(defvar *spline* nil
  "Current spline, rebuilt after each new knot")

(defvar *animating?* nil
  "Flag for if we're currently animating")

(defvar *drawing-path?* nil
  "Flag for if we're drawing the spline path")

(defparameter *grey* (sdl:color :r 127 :g 127 :b 127))

(defparameter *font* (make-instance 'sdl:ttf-font-definition
                                    :size 16
                                    :filename (merge-pathnames "Vera.ttf" sdl:*default-font-path*)))
(defun launch ()
  "Wrapped main launcher for better REPL experience."
  (bt:make-thread
    (lambda ()
      (main nil :quit? nil))
    :name "spline-example-main"))

(defun main (argv &key (quit? t))
  (declare (ignorable argv))
  (handler-bind
    ((serious-condition (lambda (c)
                          (uiop:print-condition-backtrace c :count 15)
                          (when quit? (uiop:quit 1)))))

    (sdl:with-init ()
      (sdl:window 800 600 :title-caption "Spline Examples" :double-buffer t :hw t)
      (setf (sdl:frame-rate) 60)
      (unless (sdl:initialise-default-font *font*) ;sdl:*ttf-font-vera*)
        (error "Couldn't initialize the default font"))
      (setup-sprite)
      (clock-tick)
      (handle-events)))
  (when quit? (uiop:quit 0)))

(defun setup-sprite ()
  (setf *sprite* (sdl:create-surface 5 5))
  (sdl:fill-surface sdl:*red* :surface *sprite*))

(defun handle-events ()
  (sdl:with-events ()
    (:quit-event () t)
    (:key-down-event (:key key)
     (handle-keydown key))
    (:mouse-button-up-event (:button button :x x :y y)
     (handle-click button x y))
    (:idle ()
     (game-tick))))

(defun handle-click (button x y)
  (when (= button 1)
    (if (null *knots*) (push (vector x y) *knots*) (nconc *knots* (list (vector x y))))
    (when (sufficient-knots?)
      (init-spline))))

(defun init-spline (&optional (dt *dt*))
  (setf *spline* (make-instance 'spline :dt dt))
  (loop for knot in *knots* do
        (add-knot *spline* knot)))

(defun sufficient-knots? ()
  "Need at least 3 knots to make a spline."
  (>= (length *knots*) 3))

(defun handle-keydown (key)
  (cond
    ((or (sdl:key= key :sdl-key-escape)
         (sdl:key= key :sdl-key-q))
     (sdl:push-quit-event))

    ((sdl:key= key :sdl-key-c)
     (setf *spline* nil)
     (setf *animating?* nil)
     (setf *knots* (list)))

    ((sdl:key= key :sdl-key-1) ; small hourglass
     (setf *knots* (list #(190 40) #(130 40) #(160 70) #(190 100) #(130 100) #(160 70)))
     (init-spline))
    ((sdl:key= key :sdl-key-2) ; scaled hourglass (could also (scale *spline* 2))
     (setf *knots* (list #(380 80) #(260 80) #(320 140) #(380 200) #(260 200) #(320 140)))
     (init-spline))
    ((sdl:key= key :sdl-key-3) ; infinity/bowtie
     (setf *knots* (list #(200 350) #(340 445) #(515 520) #(515 350) #(340 445) #(200 520) #(200 350)))
     (init-spline))
    ((sdl:key= key :sdl-key-4) ; box
     (setf *knots* (list #(180 40) #(120 40) #(120 90) #(180 90) #(180 40)))
     (init-spline))
    ((sdl:key= key :sdl-key-5) ; zag
     (setf *knots* (list #(160 20) #(150 40) #(160 60) #(150 80) #(160 100) #(150 100) #(160 80) #(150 60) #(160 30)))
     (init-spline 0.05))
    ((sdl:key= key :sdl-key-6) ; "attack" -- suppose there's a player at 20, 40 and we spawn at 200, 100. This makes a sort of attack pattern on player's pos. Player will likely try to avoid it, so after reaching close to (old) player pos, fall back a bit
     (let ((p-x 20)
           (p-y 40)
           (s-x 200)
           (s-y 100))
       (setf *knots* (list `#(,s-x ,s-y) `#(,(- s-x 20) ,s-y) `#(,(+ p-x 80) ,(+ p-y 10)) `#(,(- p-x 5) ,p-y) `#(,(+ p-x 60) ,(- p-y 10)) #(140 70) #(150 80) #(140 90)))
       (init-spline 0.05)))
    ((sdl:key= key :sdl-key-7) ; "wavey" around a spawn point
     (let ((s-x 300)
           (s-y 250))
       (setf *knots* (list `#(,s-x ,s-y) `#(,(- s-x 30) ,(+ s-y 10)) `#(,(- s-x 60) ,s-y) `#(,(- s-x 90) ,(- s-y 10)) `#(,(- s-x 120) ,s-y)))
       (init-spline 0.1)))
    ((sdl:key= key :sdl-key-8) ; fish
     (setf *knots* (list #(86 343) #(355 530) #(642 421) #(361 317) #(99 531)))
     (init-spline))
    ((sdl:key= key :sdl-key-9) ; roller coaster -- interesting overshoot near the end, try adding a point in the gap betweeen for even more pronounced effect
     (setf *knots* (list #(87 475) #(139 420) #(200 355) #(270 285) #(334 220)   #(400 221) #(668 498)))
     (init-spline))
    ; cool example I didn't get around to:
    ; first, do a circular lap around the screen
    ; then using scaling + higher dt, shrink the circle to smaller and smaller laps
    ; finally at the small circle size, slowly (at dt) go up towards e.g. 10,200 then when reached it,
    ; use third value to change dt to be faster, go quickly down to 400,600 ish then change
    ; dt a last time to even faster, resulting in a fast check-mark action


    ((and (sdl:key= key :sdl-key-a)
          (sufficient-knots?))
     (setf *animating?* t))

    ((sdl:key= key :sdl-key-h)
     (setf *hide-help?* (not *hide-help?*)))

    ((sdl:key= key :sdl-key-i)
     (setf *hide-knots?* (not *hide-knots?*)))

    ((sdl:key= key :sdl-key-p)
     (format t "~%~s~%" *knots*)
     (loop for knot in *knots* do
           (format t "(add-knot spline ~s)~%" knot)))

    ((sdl:key= key :sdl-key-d)
     (setf *drawing-path?* (not *drawing-path?*)))
    ))

(defun game-tick ()
  (sdl:clear-display sdl:*white*)
  (clock-tick)
  (render)
  (sdl:update-display))

(defun render ()
  (unless *hide-help?*
    (sdl:draw-string-solid-* "Click to add spline knots" 20 20 :color sdl:*black*)
    (sdl:draw-string-solid-* "Press C to clear knots" 20 40 :color sdl:*black*)
    (sdl:draw-string-solid-* "Press 1-9 for pre-loaded knot demos" 20 60 :color sdl:*black*)
    (sdl:draw-string-solid-* "Press A to animate an object over the spline" 20 80 :color (if (sufficient-knots?) sdl:*black* *grey*))
    (sdl:draw-string-solid-* "Press P to print knot values" 20 100 :color sdl:*black*)
    (sdl:draw-string-solid-* "Press D to toggle drawing the spline path" 20 120 :color (if (sufficient-knots?) sdl:*black* *grey*))
    (sdl:draw-string-solid-* "Press I to toggle display of the knots" 20 140 :color sdl:*black*)
    (sdl:draw-string-solid-* "Press H to toggle display of this help" 20 160 :color sdl:*black*))

  (unless *hide-knots?*
    (loop for knot in *knots* do
          (sdl-gfx:draw-filled-circle (sdl:point :x (+ 3 (aref knot 0)) :y (+ 3 (aref knot 1))) 6 :color sdl:*black*)))

  (when (and *drawing-path?* (sufficient-knots?) (not *animating?*))
    (let ((vertices (list (first *knots*))))
      (loop for next-point = (next-point *spline*)
            do (push (map 'vector #'round next-point) vertices)
            until (equalp next-point #(0 0)))
      (pop vertices)
      (reset *spline*)
      ; writing this part I noticed sdl-gfx has a spline system too, like directX does.
      ; it's neat to see the curves matching, apart from gfx sometimes not finishing the last segment for some reason
      ;(sdl-gfx:draw-curve (coerce (cm-spline::knots *spline*) 'list) :color sdl:*green*)
      (sdl-gfx:draw-shape vertices :color sdl:*blue*)
      ))

  (when *animating?*
    (multiple-value-bind (next-point reached-end?) (next-point *spline*)
      (when reached-end?
        (setf *animating?* nil)
        (reset *spline*)
        (return-from render))
      (let ((x (round (aref next-point 0)))
            (y (round (aref next-point 1))))
        (sdl:set-position *sprite* (vector x y))
        (sdl:blit-surface *sprite*)))))


(defvar *prev-frame-time* 0.0)
(defvar *dt* 0.0)

(defun now ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun clock-tick ()
  "Updates *dt* to be the time between now and the previous frame in seconds."
  (let ((now (now)))
    (setf *dt* (- now *prev-frame-time*))
    (setf *prev-frame-time* now)))

