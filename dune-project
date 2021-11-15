(lang dune 2.8)

(name sqml)

(version 0.4.0)

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
  (acid
   (and
    (>= 0.6.0)
    (< 0.7.0)))
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
  (lwt_ppx
   (>= 2.0.0))
  (ocamlformat
   (and
    (= 0.17.0)
    :dev))
  (ptime
   (>= 0.8.5))
  (semver
   (>= 0.1.0))
  (timmy
   (and
    (>= 0.11.0)
    (< 0.12.0)))))

(package
 (name sqml-caqti)
 (synopsis "SQML Caqti backend engine.")
 (depends
  (caqti-driver-postgresql
   (and
    (>= 1.6.0)
    :with-test))
  (caqti-driver-sqlite3
   (and
    (= 1.6.0)
    :with-test))
  (caqti
   (>= 1.6.0))
  (caqti-lwt
   (>= 1.6.0))
  (schematic
   (and
    (>= 0.11.0)
    (< 0.12.0)))
  (sqlite3
   (>= 5.0.1))
  (sqml
   (= :version))))
