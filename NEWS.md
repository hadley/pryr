
# pryr 0.1.3.9000


# pryr 0.1.3

* `track_copy()` no longer reports deletes as copies (#34).

* Added `is_active_binding()` (@richfitz, #33).

* Fixed think-o in `stop_list()`.

* Fixed a warning in `%<a-%` when reassigning an active binding (@leeper, #39).

* `object_size()` now supports dots in closure environments.


# pryr 0.1.1

* `address()` no longer changes `NAMED()` status of x (#24).

* Use non-internal `nonS3Methods()` where needed (#38).

* `explicit()` and `eval2()` are deprecated. Please use the lazyeval
  package instead (#27)
