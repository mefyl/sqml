open Acid

open Let.Syntax2 (Lwt_result)

module Sequence = Sqml.Utils.Sequence

module Alcotest = struct
  include Alcotest

  let ptime = testable Ptime.pp Ptime.equal
end

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.Src.set_level Logs.default (Some Logs.Debug);
  Logs.Src.set_level Sqml.Log.src (Some Logs.Debug)

let error : [ `Sqml_engine of string ] Alcotest.testable =
  Alcotest.testable
    (fun fmt (`Sqml_engine s) -> Format.pp_print_string fmt s)
    (* We don't care about the error message *)
      (fun _ _ -> true)

let zero : (unit, Model.Query.zero, _) Model.Query.result Alcotest.testable =
  Alcotest.testable
    (fun fmt Model.Query.Zero ->
      Format.pp_print_string fmt "empty query result")
    Poly.( = )

module ModelS :
  Sqml.Model.S
    with type tag = Model.tag
     and module Query = Model.Query
     and module BindTable = Model.BindTable
     and module Join = Model.Join =
  Model

let convert_error =
  let f e = `Sqml_engine (Caqti_error.show e) in
  Result.map_error ~f

module Test (Engine : Sqml.Engine.S with module Model = ModelS) = struct
  let select make () =
    let q = Model.select ~from:(module Model.T1) Model.T1.Column.id in
    Alcotest.(
      check string "repr" "SELECT table1.id FROM table1"
        (Fmt.str "%a" Model.Query.pp q));
    let* db = make () in
    let* (Many res) = Engine.query db q in
    let* res = Sequence.to_list res in
    Alcotest.(check (list int) "selected" [] res) |> Lwt_result.return

  let insert make () =
    let* db = make () in
    let* Zero =
      let q = Model.insert ~into:(module Model.T1) (0, "zero") in
      Engine.query db q
    in

    let* (Many res) =
      let q = Model.select ~from:(module Model.T1) Model.T1.Column.id in
      Engine.query db q
    in
    let* res = Sequence.to_list res in
    Alcotest.(check (list int) "inserted ids" [ 0 ] res) |> Lwt_result.return

  let join _make () =
    let module J = Model.Join (Model.T1) (Model.T2) in
    let q = Model.select ~from:(module J) (J.left Model.T1.Column.id) in
    Alcotest.(
      check string "simple"
        "SELECT l_table1.id FROM table1 l_table1 JOIN table2 r_table2"
        (Fmt.str "%a" Model.Query.pp q));
    let module J = Model.Join (Model.T1) (Model.T1) in
    let q = Model.select ~from:(module J) (J.right Model.T1.Column.id) in
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
    let* Zero =
      let q = Model.insert ~into:(module Model.Ts) now in
      Engine.query db q
    in
    let* db = make () in
    let* Zero =
      let q = Model.insert ~into:(module Model.Ts) later in
      Engine.query db q
    in
    let q = Model.select ~from:(module Model.Ts) Model.Ts.Column.time in
    let* (Many res) = Engine.query db q in
    let* res = Sequence.to_list res in
    Lwt_result.return
    @@ Alcotest.(check (list ptime) "inserted timestamps" [ now; later ] res)

  let conflict make () =
    let* db = make () in
    let* Zero =
      let q = Model.insert ~into:(module Model.T1) (0, "zero") in
      Engine.query db q
    in
    let* () =
      let q = Model.insert ~into:(module Model.T1) (0, "zero") in
      let%lwt res = Engine.query db q in
      Lwt_result.return
        Alcotest.(
          check (result zero error) "conflict result"
            (Result.Error (`Sqml_engine ""))
            res)
    in
    let* Zero =
      let q =
        Model.insert ~into:(module Model.T1) ~on_conflict:Ignore (0, "one")
      in
      Engine.query db q
    in
    let* (Many res) =
      let q = Model.select ~from:(module Model.T1) Model.T1.Column.name in
      Engine.query db q
    in
    let* res = Sequence.to_list res in
    let () = Alcotest.(check (list string) "untouched row" [ "zero" ] res) in
    let* Zero =
      let q =
        Model.insert ~into:(module Model.T1) ~on_conflict:Update (0, "two")
      in
      Engine.query db q
    in
    let* (Many res) =
      let q = Model.select ~from:(module Model.T1) Model.T1.Column.name in
      Engine.query db q
    in
    let* res = Sequence.to_list res in
    let () = Alcotest.(check (list string) "updated row" [ "two" ] res) in
    Lwt_result.return ()

  module Versioning = struct
    let stable make () =
      let* _ = make () in
      let* _ = make () in
      Lwt_result.return ()
  end
end

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
             test_case "conflict" @@ make Caqti_test.conflict;
             test_case "timestamp" @@ make Caqti_test.timestamp;
             test_case "versioning-stable" @@ make Caqti_test.Versioning.stable;
           ] );
         ( "Caqti-PostgreSQL",
           let make db test () =
             let pg_host =
               Sys.getenv "PG_HOST" |> Option.value ~default:"localhost"
             in
             let* c =
               Caqti_lwt.connect
                 (Uri.of_string @@ "postgres://postgres@" ^ pg_host)
               |> Lwt.map ~f:convert_error
             in
             let module C = (val c) in
             let* () =
               C.exec
                 (Caqti_request.exec Caqti_type.unit
                 @@ Format.sprintf "CREATE DATABASE %S" db)
                 ()
               |> Lwt.map ~f:convert_error
             in
             let open Let.Syntax (Lwt) in
             let* res =
               Lwt_switch.with_switch (fun switch ->
                   let open Let.Syntax2 (Lwt_result) in
                   let make () =
                     let+ res =
                       Caqti.make ("postgres://postgres@" ^ pg_host ^ "/" ^ db)
                     in
                     let () =
                       Lwt_switch.add_hook (Some switch) (fun () ->
                           Caqti.close res)
                     in
                     res
                   in
                   test make ())
             in
             let* drop_res =
               C.exec
                 (Caqti_request.exec Caqti_type.unit
                 @@ Format.sprintf "DROP DATABASE %S" db)
                 ()
               |> Lwt.map ~f:convert_error
             in
             Result.map ~f:(fun ((), ()) -> ()) (Result.both res drop_res)
             |> Lwt.return
           in
           [
             test_case "select" @@ make "select" Caqti_test.select;
             test_case "insert" @@ make "insert" Caqti_test.insert;
             test_case "join" @@ make "join" Caqti_test.join;
             test_case "timestamp" @@ make "timestamp" Caqti_test.timestamp;
             test_case "conflict" @@ make "conflict" Caqti_test.conflict;
             test_case "versioning-stable"
             @@ make "versioning_stable" Caqti_test.Versioning.stable;
           ] );
       ]
