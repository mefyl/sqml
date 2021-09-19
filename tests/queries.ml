open Acid

open Let.Syntax2 (Lwt_result)

module Sequence = Sqml.Utils.Sequence

module Alcotest = struct
  include Alcotest

  let time = Alcotest.testable Timmy.Time.pp Timmy.Time.equal
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

let t1 =
  Alcotest.(
    testable
      (fun f (i, name, desc) ->
        Fmt.(pf f "(%i, %s, %a)" i name (option string) desc))
      Poly.equal)

module Test (Engine : Sqml.Engine.S with module Model = ModelS) = struct
  let default_t1s =
    [ (0, "zero", None); (1, "one", Some "1"); (2, "two", Some "2") ]

  let populate ?(t1s = default_t1s) db =
    let f t1 =
      let q =
        Model.insert
          ~into:(module Model.T1)
          Sqml.Expression.(const Model.T1.type_ t1)
      in
      let+ Zero = Engine.query db q in
      ()
    in
    Lwt_list.map_p f t1s |> Lwt.map ~f:Result.all_unit

  let check_t1s db expected =
    let* (Many all) =
      Engine.query db @@ Model.select ~from:(module Model.T1) Model.T1.id
    in
    let+ all = Sequence.to_list all in
    Alcotest.(check (list int) "ids" expected all)

  let select make () =
    let q = Model.select ~from:(module Model.T1) Model.T1.id
    and q_all = Model.select ~from:(module Model.T1) Model.T1.all in
    let* db = make () in
    let* () =
      let* (Many res) = Engine.query db q in
      let* res = Sequence.to_list res in
      Alcotest.(check (list int) "selected" [] res) |> Lwt_result.return
    and* () =
      let* (Many res) = Engine.query db q_all in
      let* res = Sequence.to_list res in
      Alcotest.(check (list t1) "selected" [] res) |> Lwt_result.return
    in
    let* () = populate db in
    let* () =
      let* (Many res) = Engine.query db q in
      let* res = Sequence.to_list res in
      Alcotest.(
        check (list int) "selected ids" [ 0; 1; 2 ]
          (List.sort ~compare:Int.compare res))
      |> Lwt_result.return
    and* () =
      let* (Many res) = Engine.query db q_all in
      let* res = Sequence.to_list res in
      Alcotest.(
        check (list t1) "selected t1s" default_t1s
        @@ List.sort ~compare:(fun (l, _, _) (r, _, _) -> Int.compare l r) res)
      |> Lwt_result.return
    in
    Lwt_result.return ()

  let insert make () =
    let* db = make () in
    let* Zero =
      let q =
        Model.insert ~into:(module Model.T1) @@ Model.T1.const (0, "zero", None)
      in
      Engine.query db q
    in
    let* (Many res) =
      let q = Model.select ~from:(module Model.T1) Model.T1.id in
      Engine.query db q
    in
    let* res = Sequence.to_list res in
    Alcotest.(check (list int) "inserted ids" [ 0 ] res) |> Lwt_result.return

  let delete make () =
    let* db = make () in
    let delete where =
      let+ Zero =
        Engine.query db @@ Model.delete ~from:(module Model.T1) ?where ()
      in
      ()
    in
    let* () = populate db in
    let* () = delete (Some Sqml.Expression.(Model.T1.id = int 1)) in
    let* () = check_t1s db [ 0; 2 ] in
    let* () = delete (Some Sqml.Expression.(Model.T1.id = int 3)) in
    let* () = check_t1s db [ 0; 2 ] in
    let* () = delete None in
    check_t1s db []

  let delete_returning make () =
    let* db = make () in
    let delete where =
      let* (Many res) =
        Engine.query db
        @@ Model.delete_returning ~from:(module Model.T1) ?where Model.T1.name
      in
      Sequence.to_list res
    in
    let* () = populate db in
    let* res = delete (Some Sqml.Expression.(Model.T1.id = int 1)) in
    let () = Alcotest.(check (list string) "deleted rows" [ "one" ] res) in
    let* () = check_t1s db [ 0; 2 ] in
    let* res = delete (Some Sqml.Expression.(Model.T1.id = int 3)) in
    let () = Alcotest.(check (list string) "deleted rows" [] res) in
    let* () = check_t1s db [ 0; 2 ] in
    let* res = delete None in
    let () =
      Alcotest.(check (list string) "deleted rows" [ "zero"; "two" ] res)
    in
    check_t1s db []

  let timestamp make () =
    let now =
      Ptime_clock.now () |> Ptime.truncate ~frac_s:0 |> Timmy.Time.of_ptime
    in
    let later = Timmy.Time.(now + Timmy.Span.seconds 3600) in
    let* db = make () in
    let* Zero =
      let q = Model.insert ~into:(module Model.Ts) @@ Model.Ts.const now in
      Engine.query db q
    in
    let* db = make () in
    let* Zero =
      let q = Model.insert ~into:(module Model.Ts) @@ Model.Ts.const later in
      Engine.query db q
    in
    let q = Model.select ~from:(module Model.Ts) Model.Ts.time in
    let* (Many res) = Engine.query db q in
    let* res = Sequence.to_list res in
    Lwt_result.return
    @@ Alcotest.(check (list time) "inserted timestamps" [ now; later ] res)

  let conflict psql make () =
    let* db = make () in
    let* Zero =
      let q =
        Model.insert ~into:(module Model.T1) @@ Model.T1.const (0, "zero", None)
      in
      Engine.query db q
    in
    let* () =
      let q =
        Model.insert ~into:(module Model.T1) @@ Model.T1.const (0, "zero", None)
      in
      let%lwt res = Engine.query db q in
      Lwt_result.return
        Alcotest.(
          check (result zero error) "conflict result"
            (Result.Error (`Sqml_engine ""))
            res)
    in
    let* Zero =
      let q =
        Model.insert ~into:(module Model.T1) ~on_conflict:Ignore
        @@ Model.T1.const (0, "one", Some "1")
      in
      Engine.query db q
    in
    let* (Many res) =
      let q = Model.select ~from:(module Model.T1) Model.T1.name in
      Engine.query db q
    in
    let* res = Sequence.to_list res in
    let () = Alcotest.(check (list string) "untouched row" [ "zero" ] res) in
    let* Zero =
      let q =
        Model.insert ~into:(module Model.T1) ~on_conflict:Update
        @@ Model.T1.const (0, "two", Some "2")
      in
      Engine.query db q
    in
    let* (Many res) =
      let q = Model.select ~from:(module Model.T1) Model.T1.name in
      Engine.query db q
    in
    let* res = Sequence.to_list res in
    let () = Alcotest.(check (list string) "updated row" [ "two" ] res) in
    let* () =
      let open Sqml.Expression in
      if psql then
        let* Zero =
          let q =
            Model.insert
              ~into:(module Model.T1)
              ~on_conflict:
                (Set
                   (fun { excluded } ->
                     [
                       Model.T1.Column.name := Model.T1.name ^ string "'";
                       Model.T1.Column.description :=
                         excluded Model.T1.description;
                     ]))
            @@ Model.T1.const (0, "three", Some "3")
          in
          Engine.query db q
        in
        let* (Many res) =
          let q =
            Model.select
              ~from:(module Model.T1)
              Model.T1.(name @ description @ empty_list)
          in
          Engine.query db q
        in
        Sequence.to_list res >>| function
        | [ Sqml.H_list.[ name; description ] ] ->
          Alcotest.(check string "updated row name" "two'" name);
          Alcotest.(
            check (option string) "updated row description" (Some "3")
              description)
        | _ -> Alcotest.fail "wrong number of lines"
      else
        Lwt_result.return ()
    in
    Lwt_result.return ()

  let prepared make () =
    let* db = make () in
    let get id =
      Model.select
        ~from:(module Model.T1)
        ~where:Sqml.Expression.(Model.T1.id = id)
        Model.T1.name
    and put t1 = Model.insert ~into:(module Model.T1) t1 in
    let get = Engine.prepare Sqml.Type.(Atomic Int) get
    and put = Engine.prepare Model.T1.type_ put in
    let* () = Lwt_unix.sleep 5. |> Lwt.map ~f:Result.return in
    let* Zero = put db (271, "prepared", Some "prepared") in
    let* (Many res) = get db 271 in
    let* res = Sequence.to_list res in
    Alcotest.(check (list string) "selected" [ "prepared" ] res);
    let* () = Lwt_unix.sleep 5. |> Lwt.map ~f:Result.return in
    let* (Many res) = get db 271 in
    let+ res = Sequence.to_list res in
    Alcotest.(check (list string) "selected" [ "prepared" ] res)

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
             test_case "delete" @@ make Caqti_test.delete;
             test_case "delete_returning" @@ make Caqti_test.delete_returning;
             test_case "conflict" @@ make @@ Caqti_test.conflict false;
             test_case "timestamp" @@ make Caqti_test.timestamp;
             test_case "versioning-stable" @@ make Caqti_test.Versioning.stable;
             test_case "prepared" @@ make Caqti_test.prepared;
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
             test_case "delete" @@ make "delete" Caqti_test.delete;
             test_case "delete_returning"
             @@ make "delete_returning" Caqti_test.delete_returning;
             test_case "timestamp" @@ make "timestamp" Caqti_test.timestamp;
             test_case "conflict" @@ make "conflict" @@ Caqti_test.conflict true;
             test_case "versioning-stable"
             @@ make "versioning_stable" Caqti_test.Versioning.stable;
             test_case "prepared" @@ make "prepared" Caqti_test.prepared;
           ] );
       ]
