v1.7 2021-06-04 Paris
---------------------

* bugfix: fix buffer overflows by not using _unsafe ocplib-endian functions (@c-cube)
* read_all: bugfix (pos vs n_read) (@c-cube)
* test: fix for 32bit archs

v1.6 2021-03-20 Paris
---------------------

* bugfix: bug when serializing big Int values
* bugfix: 32 bits architectures now also try to pack parsed ints into int type
* ocamlformat

v1.5 2020-01-10 Paris
---------------------

* use Buffer's binary encodings of integers (>= 4.08.0)
* bugfix: 32 bits architecture now working as well
* bugfix: fix computation of size for the Bytes type

v1.4 2018-04-27 Paris
---------------------

* compile in dev mode (removed dead code and unused variables)
* add optional `Msgpck_repr` module compatible with `ocplib-json-typed`
* tests: switch to alcotest
* add compare and equal functions
* bugfix: uint64 were previously written as int64

v1.3 2017-05-17 Paris
---------------------

* Add read_all function.
* Better error messages on unpack.
* Add format based pretty-printers.
* BUILD: switch to jbuilder.

v1.2 2017-03-06 Paris
---------------------

* BUGFIX: fix reading signed integers.

v1.1 2017-02-21 Paris
---------------------

* Add documentation.
* Drop dependency to ppx (and sexplib).
* Function `to_string' now uses `size' and allocate a string of the
  exact required length.
* Add function `size'.

v1.0 2016-08-31 Paris
---------------------

First release.
