(rule (target sqml.%{version:sqml}.opam) (deps (:opam sqml.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/sqml#%{version:sqml}\" }")))))
(rule (deps (universe)) (target sqml.opam.locked)
 (action (run %{bin:opam} lock sqml)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target sqml.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:sqml.opam.locked} --exclude sqml,sqml-caqti,acid,hlist,timmy))))
(rule (target sqml-caqti.%{version:sqml-caqti}.opam)
 (deps (:opam sqml-caqti.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/sqml#%{version:sqml-caqti}\" }")))))
(rule (deps (universe)) (target sqml-caqti.opam.locked)
 (action (run %{bin:opam} lock sqml-caqti)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target sqml-caqti.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:sqml-caqti.opam.locked} --exclude sqml,sqml-caqti,acid,hlist,timmy))))
