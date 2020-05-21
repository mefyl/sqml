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
  (base
   (>= 0.13.1))
  (fmt
   (>= 0.8.8))
  (lwt
   (>= 5.3.0))
  (ocamlformat
   (and
    (= 0.14.1)
    :dev))
  (ptime
   (>= 0.8.5))
  (semver
   (>= 0.1.0))
  (sqlite3
   (>= 5.0.1))))
