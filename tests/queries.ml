open Model
open Sqml

let select () =
  let q = Sql.select (module T1) T1.Column.id in
  Alcotest.(
    check string "simple" "SELECT table1.id FROM table1"
      (Fmt.str "%a" Query.pp q))

let join () =
  let module J = Sql.Join (T1) (T2) in
  let q = Sql.select (module J) (J.left T1.Column.id) in
  Alcotest.(
    check string "simple"
      "SELECT l_table1.id FROM table1 l_table1 JOIN table2 r_table2"
      (Fmt.str "%a" Query.pp q));
  let module J = Sql.Join (T1) (T1) in
  let q = Sql.select (module J) (J.right T1.Column.id) in
  Alcotest.(
    check string "self"
      "SELECT r_table1.id FROM table1 l_table1 JOIN table1 r_table1"
      (Fmt.str "%a" Query.pp q))

let () =
  let open Alcotest in
  let test_case name f = test_case name `Quick f in
  run "SQML"
    [ ("Select", [ test_case "select" select; test_case "join" join ]) ]
