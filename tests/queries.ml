open Acid

open Let.Syntax2 (Lwt_result)

module Sequence = Sqml.Engine.Sequence

module Alcotest = struct
  include Alcotest

  let ptime = testable Ptime.pp Ptime.equal
end

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.Src.set_level Logs.default (Some Logs.Debug)

module ModelS :
  Sqml.Model.S
    with type tag = Model.tag
     and module Query = Model.Query
     and module BindTable = Model.BindTable
     and module Join = Model.Join =
  Model

(* open Model *)

module Test (Engine : Sqml.Engine.S with module Model = ModelS) = struct
  let select make () =
    let q = Model.select (module Model.T1) Model.T1.Column.id in
    Alcotest.(
      check string "repr" "SELECT table1.id FROM table1"
        (Fmt.str "%a" Model.Query.pp q));
    let* db = make () in
    let* res = Engine.query db q in
    let* res = Sequence.to_list res in
    Alcotest.(check (list int) "selected" [] res) |> Lwt_result.return

  let insert make () =
    let* db = make () in
    let* () = Engine.insert db (module Model.T1) (0, "zero") in
    let q = Model.select (module Model.T1) Model.T1.Column.id in
    let* res = Engine.query db q in
    let* res = Sequence.to_list res in
    Alcotest.(check (list int) "inserted ids" [ 0 ] res) |> Lwt_result.return

  let join _make () =
    let module J = Model.Join (Model.T1) (Model.T2) in
    let q = Model.select (module J) (J.left Model.T1.Column.id) in
    Alcotest.(
      check string "simple"
        "SELECT l_table1.id FROM table1 l_table1 JOIN table2 r_table2"
        (Fmt.str "%a" Model.Query.pp q));
    let module J = Model.Join (Model.T1) (Model.T1) in
    let q = Model.select (module J) (J.right Model.T1.Column.id) in
    Alcotest.(
      check string "self"
        "SELECT r_table1.id FROM table1 l_table1 JOIN table1 r_table1"
        (Fmt.str "%a" Model.Query.pp q));
    Lwt_result.return ()

  let timestamp make () =
    let now = Option.value_exn (Ptime.of_float_s @@ Unix.time ()) in
    let later =
      Option.value_exn (Ptime.of_float_s @@ (Unix.time () +. 3600.))
    in
    let* db = make () in
    let* () = Engine.insert db (module Model.Ts) now in
    let* () = Engine.insert db (module Model.Ts) later in
    let q = Model.select (module Model.Ts) Model.Ts.Column.time in
    let* res = Engine.query db q in
    let* res = Sequence.to_list res in
    Lwt_result.return
    @@ Alcotest.(check (list ptime) "inserted timestamps" [ now; later ] res)

  module Versioning = struct
    let stable make () =
      let* _ = make () in
      let* _ = make () in
      Lwt_result.return ()
  end
end

let error : [ `Sqml_engine of string ] Alcotest.testable =
  Alcotest.testable
    (fun fmt (`Sqml_engine s) -> Format.pp_print_string fmt s)
    ( = )

module Sqlite = Sqml_sqlite3.Make (Model)
module Sqlite_test = Test (Sqlite)
module Caqti = Sqml_caqti.Make (Model)
module Caqti_test = Test (Caqti)

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
       [
         ( "SQLite",
           let make () =
             let db = Sqlite3.db_open ~memory:true ":memory:" in
             fun () -> Sqlite.of_db db
           in
           [
             test_case "select" @@ Sqlite_test.select (make ());
             test_case "insert" @@ Sqlite_test.insert (make ());
             test_case "join" @@ Sqlite_test.join (make ());
             test_case "timestamp" @@ Sqlite_test.timestamp (make ());
             test_case "versioning-stable"
             @@ Sqlite_test.Versioning.stable (make ());
           ] );
         ( "Caqti-SQLite",
           let make test () =
             let open Let.Syntax (Lwt) in
             let f (name, channel) =
               let* () = Lwt_io.close channel in
               let make () = Caqti.make ("sqlite3://" ^ name) in
               test make ()
             in
             Lwt_io.with_temp_file ~prefix:"sqml-test" f
           in
           [
             test_case "select" @@ make Caqti_test.select;
             test_case "insert" @@ make Caqti_test.insert;
             test_case "join" @@ make Caqti_test.join;
             test_case "timestamp" @@ make Caqti_test.timestamp;
             test_case "versioning-stable" @@ make Caqti_test.Versioning.stable;
           ] );
       ]
