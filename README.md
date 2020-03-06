# CL-Catmull-Rom-Spline

This is a Common Lisp library implementing Catmull-Rom splines as described by
this paper: http://algorithmist.net/docs/catmullrom.pdf

Splines provide a way to designate an ordered list of control points (knots) in
a 2D plane and find a "smooth" path that travels through or around the points.
CM splines have particularly nice properties for game applications:

* The path actually goes through each knot
* If you relocate a knot then only the path near the knot changes
* You can have a looping path by using the same knot as the start and end point
* Paths (usually) look nicely smooth enough despite not being C²-continuous like
  cubic splines
* Cheap to calculate

![example.gif](example/example.gif)

# System Outline

`:cl-catmull-rom-spline` is the main library code. The typical usage is to
construct a new Spline object, add some knots, then repeatedly ask for the next
point along the spline path.

`:cl-catmull-rom-spline/test` contains a few unit tests.

`:cl-catmull-rom-spline/example` contains a small SDL1-backed graphical example
to show off adding knots and animating an object following the path. Simply
load the system and execute `(spline-example:launch)`.
Note that testing on an Ubuntu 16 system, even after apt-getting libsdl1.2-dev,
this system was raising an error on load trying to find libSDL\_gfx.
Continuing the load and launching still seemed to work anyway, though. But
it's also resolveable by apt-getting libsdl-gfx1.2-dev.

# License

This library is free software. The author disclaims copyright to this project's
source code. All files in this project, unless explicitly stated otherwise, are
in the public domain and distributed without any warranty. If you live in a
country that does not recognize grants to the public domain, you may consider
this licensed under the
[CC0](https://creativecommons.org/share-your-work/public-domain/cc0/).

# Contributing

Though responses may not be very quick, contributions from pull requests or
emailed patches are welcome, as are drive-by code reviews or reported issues.
If you don't agree to have the changes released in the public domain, please
mark them explicitly otherwise.
