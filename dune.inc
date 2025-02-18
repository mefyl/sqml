(alias (name extdeps) (deps opam/sqml.opam.extdeps))
(subdir opam
 (rule (target sqml.%{version:sqml}.opam) (deps (:opam ../sqml.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml.opam.locked)
  (action (run %{bin:opam} lock sqml)))
 (rule (mode (promote (until-clean))) (target sqml.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy))))
 (rule (target sqml-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../sqml.opam} --cross
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --cross-both ""
     --cross-exclude ""))))
 (rule (target sqml-ios.%{version:sqml}.opam) (deps (:opam sqml-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-ios.opam.locked)
  (action (run %{bin:opam} lock sqml-ios)))
 (rule (mode (promote (until-clean))) (target sqml.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-ios.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --toolchain ios))))
 (rule (target sqml-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../sqml.opam} --cross
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --cross-both ""
     --cross-exclude ""))))
 (rule (target sqml-macos.%{version:sqml}.opam)
  (deps (:opam sqml-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-macos.opam.locked)
  (action (run %{bin:opam} lock sqml-macos)))
 (rule (mode (promote (until-clean))) (target sqml.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-macos.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --toolchain macos))))
 (rule (target sqml-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../sqml.opam} --cross
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --cross-both ""
     --cross-exclude ""))))
 (rule (target sqml-windows.%{version:sqml}.opam)
  (deps (:opam sqml-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-windows.opam.locked)
  (action (run %{bin:opam} lock sqml-windows)))
 (rule (mode (promote (until-clean))) (target sqml.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-windows.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --toolchain
     windows))))
 (rule (target sqml-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../sqml.opam} --cross
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --cross-both ""
     --cross-exclude ""))))
 (rule (target sqml-android.%{version:sqml}.opam)
  (deps (:opam sqml-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-android.opam.locked)
  (action (run %{bin:opam} lock sqml-android)))
 (rule (mode (promote (until-clean))) (target sqml.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-android.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --toolchain
     android)))))
(alias (name extdeps) (deps opam/sqml-caqti.opam.extdeps))
(subdir opam
 (rule (target sqml-caqti.%{version:sqml-caqti}.opam)
  (deps (:opam ../sqml-caqti.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-caqti.opam.locked)
  (action (run %{bin:opam} lock sqml-caqti)))
 (rule (mode (promote (until-clean))) (target sqml-caqti.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-caqti.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy))))
 (rule (target sqml-caqti-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../sqml-caqti.opam} --cross
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --cross-both ""
     --cross-exclude ""))))
 (rule (target sqml-caqti-ios.%{version:sqml-caqti}.opam)
  (deps (:opam sqml-caqti-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-caqti-ios.opam.locked)
  (action (run %{bin:opam} lock sqml-caqti-ios)))
 (rule (mode (promote (until-clean))) (target sqml-caqti.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-caqti-ios.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --toolchain ios))))
 (rule (target sqml-caqti-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../sqml-caqti.opam} --cross
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --cross-both ""
     --cross-exclude ""))))
 (rule (target sqml-caqti-macos.%{version:sqml-caqti}.opam)
  (deps (:opam sqml-caqti-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-caqti-macos.opam.locked)
  (action (run %{bin:opam} lock sqml-caqti-macos)))
 (rule (mode (promote (until-clean))) (target sqml-caqti.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-caqti-macos.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --toolchain macos))))
 (rule (target sqml-caqti-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../sqml-caqti.opam} --cross
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --cross-both ""
     --cross-exclude ""))))
 (rule (target sqml-caqti-windows.%{version:sqml-caqti}.opam)
  (deps (:opam sqml-caqti-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-caqti-windows.opam.locked)
  (action (run %{bin:opam} lock sqml-caqti-windows)))
 (rule (mode (promote (until-clean)))
  (target sqml-caqti.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-caqti-windows.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --toolchain
     windows))))
 (rule (target sqml-caqti-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../sqml-caqti.opam} --cross
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --cross-both ""
     --cross-exclude ""))))
 (rule (target sqml-caqti-android.%{version:sqml-caqti}.opam)
  (deps (:opam sqml-caqti-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/sqml#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target sqml-caqti-android.opam.locked)
  (action (run %{bin:opam} lock sqml-caqti-android)))
 (rule (mode (promote (until-clean)))
  (target sqml-caqti.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:sqml-caqti-android.opam.locked} --local
     sqml,sqml-caqti,acid,acid-lwt,hlist,metaversion,timmy --toolchain
     android)))))
