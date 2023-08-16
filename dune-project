(lang dune 2.8)

(name sqml)

(version 0.10.5)

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
    (>= 0.11.0)
    (< 0.12.0)))
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
    (< 1.1.0)))))

(package
 (name sqml-caqti)
 (synopsis "SQML Caqti backend engine.")
 (depends
  (caqti
   (and
    (>= 1.8.0)
    (< 1.9.0)))
  (caqti-driver-postgresql :with-test)
  (caqti-driver-sqlite3 :with-test)
  caqti-lwt
  (sqlite3
   (>= 5.0.1))
  (sqml
   (= :version))))
