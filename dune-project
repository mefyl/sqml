(lang dune 3.20)

(name sqml)

(version 0.11.1)

(generate_opam_files true)

(license GPLv3)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(source
 (github mefyl/sqml))

(package
 (name sqml)
 (synopsis "Type safe SQL queries.")
 (depends
  (acid-lwt
   (and
    (>= 0.13.0)
    (< 0.14.0)))
  (alcotest-lwt
   (and
    (>= 1.1.0)
    :with-test))
  ezjsonm
  (fmt
   (>= 0.8.8))
  logs
  (lwt
   (>= 5.3.0))
  ppx_sexp_conv
  (ptime
   (>= 0.8.5))
  semver2
  (timmy
   (and
    (>= 1.0.1)
    (< 2.0.0)))))

(package
 (name sqml-caqti)
 (synopsis "SQML Caqti backend engine.")
 (depends
  (caqti
   (and
    (>= 2.0.0)
    (< 2.1.0)))
  caqti-driver-postgresql
  (caqti-driver-sqlite3 :with-test)
  caqti-lwt
  (sqlite3
   (>= 5.0.1))
  (sqml
   (= :version))))

(package
 (name sqml-schematic)
 (synopsis "SQML schematic plugin.")
 (depends
  (schematic
   (and
    (>= 0.21.0)
    (< 0.22.0)))
  (sqml
   (= :version))))
