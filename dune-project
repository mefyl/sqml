(lang dune 2.5)

(name sqml)

(generate_opam_files true)

(license GPLv3)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(source
 (github mefyl/sqml))

(package
 (name sqml)
 (depends
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (caqti-driver-sqlite3
   (and
    (>= 1.2.1)
    :with-test))
  (caqti-driver-postgresql
   (and
    (>= 1.2.1)
    :with-test))
  (caqti
   (>= 1.2.0))
  (caqti-lwt
   (>= 1.2.0))
  (fmt
   (>= 0.8.8))
  (lwt
   (>= 5.3.0))
  (lwt_ppx
   (>= 2.0.0))
  (ocamlformat
   (and
    (= 0.15.0)
    :dev))
  (ptime
   (>= 0.8.5))
  (semver
   (>= 0.1.0))
  (sqlite3
   (>= 5.0.1))))
