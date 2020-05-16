open Acid
open Model
module Sqlite3 = Sqml_sqlite3.Make (Model)

let ( let* ) = Lwt_result.( >>= )

module Sequence = Sqml.Engine.Sequence

let select () =
  let q = Model.select (module T1) T1.Column.id in
  Alcotest.(
    check string "repr" "SELECT table1.id FROM table1" (Fmt.str "%a" Query.pp q));
  let* db = Sqlite3.make () in
  let* res = Sqlite3.query db q in
  let* res = Sequence.to_list res in
  Alcotest.(check (list int) "tmp" [] res) |> Lwt_result.return

let join () =
  let module J = Model.Join (T1) (T2) in
  let q = Model.select (module J) (J.left T1.Column.id) in
  Alcotest.(
    check string "simple"
      "SELECT l_table1.id FROM table1 l_table1 JOIN table2 r_table2"
      (Fmt.str "%a" Query.pp q));
  let module J = Model.Join (T1) (T1) in
  let q = Model.select (module J) (J.right T1.Column.id) in
  Alcotest.(
    check string "self"
      "SELECT r_table1.id FROM table1 l_table1 JOIN table1 r_table1"
      (Fmt.str "%a" Query.pp q));
  Lwt_result.return ()

let error : [ `Sqml_engine of string ] Alcotest.testable =
  Alcotest.testable
    (fun fmt (`Sqml_engine s) -> Format.pp_print_string fmt s)
    ( = )

let () =
  let open Alcotest_lwt in
  let test_case name f =
    let f _ () =
      Lwt.map
        ~f:Alcotest.(check (result unit error) "result" (Result.Ok ()))
        (f ())
    in

    test_case name `Quick f
  in
  Lwt_main.run
  @@ run "SQML"
       [ ("Select", [ test_case "select" select; test_case "join" join ]) ]
