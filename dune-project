(lang dune 3.20)

(name sqml)

(version 0.12.0)

(generate_opam_files true)

(license GPLv3)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(source
 (github mefyl/sqml))

(implicit_transitive_deps false)

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
  (lwt
   (>= 5.3.0))
  ppx_sexp_conv
  (ptime
   (>= 0.8.5))
  semver2
  stere
  (timmy
   (and
    (>= 1.0.1)
    (< 2.0.0)))
  (timmy-unix :with-test)))

(package
 (name sqml-caqti)
 (synopsis "SQML Caqti backend engine.")
 (depends
  (caqti
   (and
    (>= 2.0.0)
    (< 3.0.0)))
  caqti-driver-postgresql
  (caqti-driver-sqlite3 :with-test)
  caqti-lwt
  (sqlite3
   (>= 5.0.1))
  (schematic
   (and
    (>= 0.22.0)
    (< 0.23.0)))
  (sqml
   (= :version))))

(package
 (name sqml-schematic)
 (synopsis "SQML schematic plugin.")
 (depends
  (schematic
   (and
    (>= 0.22.0)
    (< 0.23.0)))
  (sqml
   (= :version))))

(pin
 (url
  git+https://github.com/dbuenzli/bytesrw#47bdebcbc527c264572fb5fbcc5ca606e67bcd32)
 (package
  (name bytesrw)
  (version 0.3.0)))
