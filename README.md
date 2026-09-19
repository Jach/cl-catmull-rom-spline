# CL-Catmull-Rom-Spline
[![Build Status](https://jenkins.thejach.com/buildStatus/icon?job=cl-catmull-rom-spline&style=plastic)](https://jenkins.thejach.com/job/cl-catmull-rom-spline/)

This is a Common Lisp library implementing Catmull-Rom splines as described by
[this paper](https://web.archive.org/web/20160910075808if_/http://algorithmist.net:80/docs/catmullrom.pdf)

Splines provide a way to designate an ordered list of control points (knots) in
a 2D plane and find a "smooth" path that travels through or around the points.
CR splines have particularly nice properties for game applications:

* The path actually goes through each knot
* If you relocate a knot then only the path near the knot changes
* You can have a looping path by using the same knot as the start and end point
* Paths (usually) look nicely smooth enough despite not being C²-continuous like
  cubic splines
* Cheap to calculate

![example.gif](example/example.gif)

# System Outline

`"cl-catmull-rom-spline"` is the main library code package. The typical usage is to
construct a new Spline object, add some knots to it, then repeatedly ask for the `next-point`
along the spline path until the end is reached.

`"cl-catmull-rom-spline/test"` contains a few unit tests.

`"cl-catmull-rom-spline/example"` contains a small SDL1-backed graphical example
to show off adding knots and animating an object following the path. Simply
load the system and execute `(spline-example:launch)`, or simply run `sbcl --script run-example.lisp`.

Note that testing on an Ubuntu 16 system, even after apt-getting libsdl1.2-dev,
this system was raising an error on load trying to find libSDL\_gfx.
Continuing the load and launching still seemed to work anyway, though. But
it's also resolveable by apt-getting libsdl-gfx1.2-dev.

Also note that if your image has already loaded something that loads cl-sdl2, then the example will fail, you'll need to start with a fresh core.

# Changes in version 2

In February 2026 I decided to dust off this library and add some features to it. I got 90% of the way done before putting it on the shelf until September 2026.
Some changes (and test code) were LLM-assisted.

I made one minor breaking change:

* I've removed the two package nicknames `#:cr-spline` and `#:com.thejach.cl-catmull-rom-spline`.
  * The official package is thus just `#:cl-catmull-rom-spline` and users should use package-local nicknames if they would like a shorter form.

If any users get bit by this I will add them back on request.

The new features:

* `add-knot` now accepts both 2D and 3D points! (In fact any number of dimensions.)
* `:auto-close?` is now an option when constructing a spline. When true, a final knot will automatically get added to be the same as the first knot to form a
  loop. If you pause the spline traversal and start adding new knots, don't worry, your knot order is preserved, the auto-close knot only ever appears after any
  knots you've added. The result is also smooth, and removes the need to care about the new `:aux-endpoint-mode` added.
* Added two additional methods to interact with a spline path. The first is by calling `(global-point-at spline time)`: treating the entire spline as a curve with
  the starting point at time=0 and end at time=1, this returns the point along the spline at a specified input time. (Thus, for example, you can easily mark the
  halfway point along the path, which may be non-obvious.) The second is by calling `(advance-by-distance spline distance)`: allowing for fixed distance
  traversal instead of fixed delta-t.
* `:alpha` is now an option when constructing a spline. The default is 0.5 which makes the spline match a catmull-rom spline. The value can be played with for
  different path looks though, e.g. as it approaches 0 it's like tightening the path through the control points.
* `:aux-endpoint-mode` is now an option when constructing a spline. The default is `:duplicate`, but you can also pass `:reflect` or `:explicit` (and in the
  latter case, manually set `.aux-start` and `.aux-end`). These also affect the spline shape in various ways: duplicating can cause kinks, reflecting avoids
  kinks but can cause pinches. Unless you've asked for explicitly set points, using `:auto-close? t` will result in a smooth loop where this mode doesn't
  matter.
* Exposed a `compute-spline` method that is explicitly for controlling performance behavior slightly better. Without calling it, the first call to `next-point`
  will be slower compared to all subsequent calls.

# Example Usage

Construct a spline, add at least three knots, then walk the path with `next-point`:

```lisp
(defpackage #:my-app
  (:use #:common-lisp)
  (:local-nicknames (#:spline #:cl-catmull-rom-spline)))
(in-package #:my-app)

(let ((spline (make-instance 'spline:spline :dt 0.1)))
  (spline:add-knot spline '(5 5))
  (spline:add-knot spline #(50 50))
  (spline:add-knot spline '(10 70))
  (loop
    (multiple-value-bind (point done? seg-end?) (spline:next-point spline)
      (when done?
        (return))
      (format t "~a~:[~; <- new knot~]~%" point seg-end?))))
```

Knots may be any sequence of real numbers so long as they all have the same length, which represents the dimensionality of the point.

The output of that example is a set of points that starts exactly on the first knot, passes through the second knot, and ends on the final knot.

```
#(5.0 5.0)
#(8.059999 7.79)
...
#(50.0 50.0) <- new knot
...
#(10.0 70.0) <- new knot
```

`:dt` sets the step size taken within a segment. The first segment takes `1/dt + 1` calls, with t running from 0 to 1 inclusive, and every later segment takes
`1/dt`, because t starts at `dt` there to avoid repeating the knot it shares with the previous segment.

Notably `next-point` returns three values:

* `point` -- the coordinate vector for this step
* `done?` -- `T` once the path has been exhausted
* `seg-end?` -- `T` when this point sits exactly on a new knot

Once the final knot has been handed back, every further call returns a zero vector with `done?` set:

```
#(7.0 8.0 9.0) done?=NIL seg-end?=T     ; the last knot
#(0 0 0)       done?=T   seg-end?=NIL
#(0 0 0)       done?=T   seg-end?=NIL
```

The zero vector matches the dimension of your knots, so it is `#(0 0)` for 2D knots and `#(0 0 0)` for 3D. It helps in writing a terminating loop where you
don't want to worry about the multiple values:

```lisp
(loop for point = (spline:next-point spline)
      until (equalp #(0 0) point)
      do ...)
```

Just be warned that if your spline path can legitimately cross #(0 0), then you'll need to check the done? flag explicitly, e.g.:

```lisp
(loop for (point done?) = (multiple-value-list (spline:next-point spline))
      until done?
      do ...)
```

`seg-end?` is useful for firing something off on arrival at a waypoint. Note that it can only ever become true if `dt` divides evenly into 1. With `:dt 0.3` a
segment steps t = 0, 0.3, 0.6, 0.9 and then moves on, so the path never lands exactly on the knots, including the final one.

Call `reset` to return the spline to the beginning and walk it again.

## Other ways to move along the path

```lisp
(spline:global-point-at spline 0.5)    ; -> #(50.0 50.0)
(spline:advance-by-distance spline 20) ; -> #(20.239315 17.945547)
```

`global-point-at` treats the whole spline as a single curve running from time 0 to time 1, which makes positions like "halfway along" easy to compute.
`advance-by-distance` moves by world-space distance rather than by `dt`, so a follower holds a constant speed through both the tight and the loose stretches of
the path; it returns a `done?` second value as well.

# License

This library is free software. The author disclaims copyright to this project's
source code. All files in this project, unless explicitly stated otherwise, are
in the public domain and distributed without any warranty. If you live in a
country that does not recognize grants to the public domain, you may consider
this licensed under the
[CC0](https://creativecommons.org/share-your-work/public-domain/cc0/) or alternatively the included UNLICENSE.

# Contributing

Though responses may not be very quick, contributions from pull requests or
emailed patches are welcome, as are drive-by code reviews or reported issues.
If you don't agree to have the changes released in the public domain, please
mark them explicitly otherwise.
