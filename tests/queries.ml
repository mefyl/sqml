open Caqti_request.Infix
open Sqml.Utils
open Let.Syntax2 (Lwt_result)
module Sequence = Sqml.Utils.Sequence

module Alcotest = struct
  include Alcotest

  let time = Alcotest.testable Timmy.Time.pp Timmy.Time.equal
end

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.Src.set_level Logs.default (Some Logs.Debug);
  (* Logs.Src.set_level Caqti_common_priv.default_log_src (Some Logs.Debug);
   * Logs.Src.set_level Caqti_common_priv.request_log_src (Some Logs.Debug); *)
  Logs.Src.set_level Sqml.Log.src (Some Logs.Debug)

let error =
  Alcotest.testable Sqml.Engine.pp_error
    (* We don't care about the error message *)
    (fun _ _ -> true)

module ModelS :
  Sqml.Model.S
    with type tag = Model.tag
     and module Query = Model.Query
     and module BindTable = Model.BindTable
     and module Join = Model.Join =
  Model

module J = Model.Join (Model.T1) (Model.T2)

module T1_T2 =
  (val J.on Sqml.Expression.(J.left Model.T1.id = J.right Model.T2.id))

let convert_error =
  let f e = `Sqml_engine (Caqti_error.show e) in
  Result.map_error ~f

let t1 = Alcotest.(testable Model.T1.pp Model.T1.equal)
let t2 = Alcotest.(testable Model.T2.pp Model.T2.equal)

module Queries = struct
  let create () =
    let buffer = Buffer.create 0 in
    let output =
      let fmt = Fmt.with_buffer buffer in
      let () = Stdlib.Format.pp_set_margin fmt 60 in
      fmt
    in
    let () =
      let queries =
        Sqml.Table.Query.create (module Model.T1)
        @ Sqml.Table.Query.create (module Model.T2)
      in
      Fmt.(
        pf output "@[<v>%a@]"
          (list ~sep:nop (Sqml.Table.Query.pp Sqml.Dialect.postgresql ++ semi))
          queries)
    and expected =
      {|CREATE TABLE "table1" (
  description TEXT,
  id INTEGER NOT NULL,
  name TEXT NOT NULL,
  CONSTRAINT table1_pkey PRIMARY KEY (id));
CREATE UNIQUE INDEX ids ON "table1" (id ASC, name DESC);
CREATE TABLE "table2" (
  id INTEGER NOT NULL,
  CONSTRAINT table2_id_fkey
    FOREIGN KEY (id) REFERENCES "table1"(id));|}
    in
    Alcotest.(
      check ~here:[%here] string "creation queries" expected
        (Buffer.contents buffer))
end

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
    Lwt_list.map_s f t1s |> Lwt.map ~f:Result.all_unit

  let populate_t2 t2s db =
    let f t2 =
      let q =
        Model.insert
          ~into:(module Model.T2)
          Sqml.Expression.(const Model.T2.type_ t2)
      in
      let+ Zero = Engine.query db q in
      ()
    in
    Lwt_list.map_s f t2s |> Lwt.map ~f:Result.all_unit

  let check_t1s db expected =
    let* (Many all) =
      Engine.query db @@ Model.select ~from:(module Model.T1) Model.T1.all
    in
    let+ all = Sequence.to_list all in

    Alcotest.(
      check ~here:[%here] (list t1) "ids" expected
      @@ List.sort ~compare:(fun (l, _, _) (r, _, _) -> Int.compare l r) all)

  let check_t1s_ids db expected =
    let* (Many all) =
      Engine.query db @@ Model.select ~from:(module Model.T1) Model.T1.id
    in
    let+ all = Sequence.to_list all in
    Alcotest.(check ~here:[%here] (list int) "ids" expected all)

  let select db =
    let q = Model.select ~from:(module Model.T1) Model.T1.id
    and q_all = Model.select ~from:(module Model.T1) Model.T1.all in
    let* () =
      let* (Many res) = Engine.query db q in
      let* res = Sequence.to_list res in
      Alcotest.(check ~here:[%here] (list int) "selected" [] res)
      |> Lwt_result.return
    in
    let* () =
      let* (Many res) = Engine.query db q_all in
      let* res = Sequence.to_list res in
      Alcotest.(check ~here:[%here] (list t1) "selected" [] res)
      |> Lwt_result.return
    in
    let* () = populate db in
    let* () =
      let* (Many res) = Engine.query db q in
      let* res = Sequence.to_list res in
      Alcotest.(
        check ~here:[%here] (list int) "selected ids" [ 0; 1; 2 ]
          (List.sort ~compare:Int.compare res))
      |> Lwt_result.return
    in
    let* () =
      let* (Many res) = Engine.query db q_all in
      let* res = Sequence.to_list res in
      Alcotest.(
        check ~here:[%here] (list t1) "selected t1s" default_t1s
        @@ List.sort ~compare:(fun (l, _, _) (r, _, _) -> Int.compare l r) res)
      |> Lwt_result.return
    in
    Lwt_result.return ()

  let insert db =
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
    Alcotest.(check ~here:[%here] (list int) "inserted ids" [ 0 ] res)
    |> Lwt_result.return

  let delete db =
    let delete where =
      let+ Zero =
        Engine.query db @@ Model.delete ~from:(module Model.T1) ?where ()
      in
      ()
    in
    let* () = populate db in
    let* () = delete (Some Sqml.Expression.(Model.T1.id = const Int 1)) in
    let* () = check_t1s_ids db [ 0; 2 ] in
    let* () = delete (Some Sqml.Expression.(Model.T1.id = const Int 3)) in
    let* () = check_t1s_ids db [ 0; 2 ] in
    let* () = delete None in
    check_t1s_ids db []

  let delete_returning db =
    let delete where =
      let* (Many res) =
        Engine.query db
        @@ Model.delete_returning ~from:(module Model.T1) ?where Model.T1.name
      in
      Sequence.to_list res
    in
    let* () = populate db in
    let* res = delete (Some Sqml.Expression.(Model.T1.id = const Int 1)) in
    let () =
      Alcotest.(check ~here:[%here] (list string) "deleted rows" [ "one" ] res)
    in
    let* () = check_t1s_ids db [ 0; 2 ] in
    let* res = delete (Some Sqml.Expression.(Model.T1.id = const Int 3)) in
    let () =
      Alcotest.(check ~here:[%here] (list string) "deleted rows" [] res)
    in
    let* () = check_t1s_ids db [ 0; 2 ] in
    let* res = delete None in
    let () =
      Alcotest.(
        check ~here:[%here] (list string) "deleted rows" [ "zero"; "two" ] res)
    in
    check_t1s_ids db []

  let update db =
    let update ?where what =
      let+ Zero =
        Engine.query db @@ Model.update (module Model.T1) ?where what
      in
      ()
    and update_returning ?where ~returning what =
      let* (Many results) =
        Engine.query db
        @@ Model.update_returning (module Model.T1) ?where ~returning what
      in
      Sequence.to_list results
    in
    let* () = populate db in
    let* () =
      update Sqml.Expression.[ Model.T1.Column.name := const String "four" ]
    in
    let* () =
      check_t1s db
        [ (0, "four", None); (1, "four", Some "1"); (2, "four", Some "2") ]
    in
    let* ids =
      update_returning
        ~where:Sqml.Expression.(Model.T1.id <= const Int 1)
        ~returning:Model.T1.name
        Sqml.Expression.
          [ Model.T1.Column.description := const (Nullable String) @@ Some "0" ]
    in
    Alcotest.(
      check ~here:[%here] (list string) "updated names" [ "four"; "four" ] ids);
    check_t1s db
      [ (0, "four", Some "0"); (1, "four", Some "0"); (2, "four", Some "2") ]

  let update_from db =
    let update table ~from ?where ~returning what =
      let* (Many results) =
        Engine.query db
        @@ Model.update_from_returning table ~from ?where ~returning what
      in
      Sequence.to_list results
    in
    let* () = populate db in
    let* () = populate_t2 [ 1; 2 ] db in
    let* res =
      let open Sqml.Expression in
      let returning Sqml.Model.{ map = mapper } = mapper Model.T1.name
      and what _ = [ Model.T2.Column.id := Model.T2.id - const Int 1 ]
      and where Sqml.Model.{ map } = map Model.T1.id = Model.T2.id in
      update (module Model.T2) ~from:(module Model.T1) ~where ~returning what
    in
    let () =
      Alcotest.(
        check ~here:[%here] (list string) "retrieved descriptions"
          [ "one"; "two" ] res)
    in
    let* res =
      let open Sqml.Expression in
      let returning Sqml.Model.{ map = mapper } =
        mapper (T1_T2.left Model.T1.name)
      and what _ = [ Model.T1.Column.name := const String "" ]
      and where Sqml.Model.{ map } =
        map (T1_T2.right Model.T2.id) = Model.T1.id
      in
      update (module Model.T1) ~from:(module T1_T2) ~where ~returning what
    in
    let () =
      Alcotest.(
        check ~here:[%here] (list string) "retrieved descriptions"
          [ "zero"; "one" ] res)
    in
    Lwt_result.return ()

  let timestamp make () =
    let now =
      Ptime_clock.now () |> Ptime.truncate ~frac_s:0 |> Timmy.Time.of_ptime
    in
    let later = Timmy.Time.(now + Timmy.Span.seconds 3600) in
    let* db = make () in
    let* Zero =
      Engine.connect db (fun db ->
          let q = Model.insert ~into:(module Model.Ts) @@ Model.Ts.const now in
          Engine.query db q)
    in
    let* db = make () in
    Engine.connect db (fun db ->
        let* Zero =
          let q =
            Model.insert ~into:(module Model.Ts) @@ Model.Ts.const later
          in
          Engine.query db q
        in
        let q = Model.select ~from:(module Model.Ts) Model.Ts.time in
        let* (Many res) = Engine.query db q in
        let* res = Sequence.to_list res in
        Lwt_result.return
        @@ Alcotest.(
             check ~here:[%here] (list time) "inserted timestamps"
               [ now; later ] res))

  let config = Alcotest.(testable Model.Configs.pp Model.Configs.equal)

  let json db =
    let baley =
      {
        Model.Configs.Owner.first_name = "Elijah";
        last_name = "Baley";
        age = 40;
      }
    and fastolfe =
      {
        Model.Configs.Owner.first_name = "Han";
        last_name = "Fastolfe";
        age = 238;
      }
    in
    let* () =
      let* Zero =
        Engine.query db
        @@ Model.insert
             ~into:(module Model.Configs)
             Sqml.Expression.(const Model.Configs.type_ ("empty", `O [], baley))
      in
      let+ Zero =
        Engine.query db
        @@ Model.insert
             ~into:(module Model.Configs)
             Sqml.Expression.(
               const Model.Configs.type_
                 ("filled", `O [ ("one", `Float 1.) ], fastolfe))
      in
      ()
    in
    let* (Many configs) =
      Engine.query db
      @@ Model.select ~from:(module Model.Configs) Model.Configs.all
    in
    let+ configs = Sequence.to_list configs in
    Alcotest.(
      check ~here:[%here] (list config) "configurations"
        [
          ("empty", `O [], baley);
          ("filled", `O [ ("one", `Float 1.) ], fastolfe);
        ]
        configs)

  let conflict psql db =
    let* Zero =
      let q =
        Model.insert ~into:(module Model.T1) @@ Model.T1.const (0, "zero", None)
      in
      Engine.query db q
    in
    let* Zero =
      let q =
        Model.insert ~into:(module Model.T1) ~on_conflict:Ignore
        @@ Model.T1.const (0, "one", Some "1")
      in
      Engine.query db q
    in
    let* (One_maybe conflicted) =
      let q =
        Model.insert_returning
          ~into:(module Model.T1)
          ~on_conflict:Ignore ~returning:Model.T1.all
        @@ Model.T1.const (0, "one", Some "1")
      in
      Engine.query db q
    in
    let () =
      Alcotest.(
        check ~here:[%here]
          (option (module Model.T1))
          "conflicting row was not inserted")
        None conflicted
    in
    let* (Many res) =
      let q = Model.select ~from:(module Model.T1) Model.T1.name in
      Engine.query db q
    in
    let* res = Sequence.to_list res in
    let () =
      Alcotest.(
        check ~here:[%here] (list string) "untouched row" [ "zero" ] res)
    in
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
    let () =
      Alcotest.(check ~here:[%here] (list string) "updated row" [ "two" ] res)
    in
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
                       Model.T1.Column.name := Model.T1.name ^ const String "'";
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
              (Sqml.Expression.of_list Model.T1.[ name; description ])
          in
          Engine.query db q
        in
        Sequence.to_list res >>| function
        | [ Hlist.[ name; description ] ] ->
          Alcotest.(check ~here:[%here] string "updated row name" "two'" name);
          Alcotest.(
            check ~here:[%here] (option string) "updated row description"
              (Some "3") description)
        | _ -> Alcotest.fail "wrong number of lines"
      else Lwt_result.return ()
    in
    Lwt_result.return ()

  let is_null db =
    let* () = populate db in
    let* (Many res) =
      let q =
        Model.select
          ~from:(module Model.T1)
          Sqml.Expression.(
            of_list
              [ is_null Model.T1.description; is_not_null Model.T1.description ])
      in
      Engine.query db q
    in
    let* res =
      Sequence.(res |> map ~f:(fun Hlist.[ l; r ] -> (l, r)) |> to_list)
    in
    let () =
      Alcotest.(
        check ~here:[%here]
          (list (pair bool bool))
          "null values"
          [ (true, false); (false, true); (false, true) ]
          res)
    in
    Lwt_result.return ()

  let prepared db =
    let get Sqml.Expression.List.[ id ] =
      Model.select
        ~from:(module Model.T1)
        ~where:Sqml.Expression.(Model.T1.id = id)
        Model.T1.name
    and put Sqml.Expression.List.[ t1 ] =
      Model.insert ~into:(module Model.T1) t1
    and update Sqml.Expression.List.[ id; name ] =
      let open Sqml.Expression in
      Model.update
        (module Model.T1)
        ~where:(Model.T1.id = id)
        [ Model.T1.Column.name := name ]
    and get_in Sqml.Expression.List.[ ids ] =
      Model.select
        ~from:(module Model.T1)
        ~where:Sqml.Expression.(in' Model.T1.id ids)
        Model.T1.name
    in
    let get = Engine.prepare [ Int ] get
    and put = Engine.prepare [ Model.T1.type_ ] put
    and update = Engine.prepare [ Int; String ] update
    and get_in = Engine.prepare [ Array Int ] get_in in
    let* Zero =
      Engine.run put db Hlist.[ (271, "prepared", Some "prepared") ]
    in
    let* (Many res) = Engine.run get db Hlist.[ 271 ] in
    let* () =
      Sequence.to_list res
      >>| Alcotest.(check ~here:[%here] (list string) "selected" [ "prepared" ])
    in
    let* Zero =
      Engine.run put db Hlist.[ (272, "prepared'", Some "prepared'") ]
    in
    let* (Many res) = Engine.run get db Hlist.[ 272 ] in
    let* () =
      Sequence.to_list res
      >>| Alcotest.(
            check ~here:[%here] (list string) "selected'" [ "prepared'" ])
    in
    let* Zero = Engine.run update db Hlist.[ 272; "prepared''" ] in
    let* (Many res) = Engine.run get db Hlist.[ 272 ] in
    let* () =
      Sequence.to_list res
      >>| Alcotest.(
            check ~here:[%here] (list string) "selected'" [ "prepared''" ])
    in
    let* (Many res) = Engine.run get_in db Hlist.[ [ 272; 273 ] ] in
    let* () =
      Sequence.to_list res
      >>| Alcotest.(
            check ~here:[%here] (list string) "selected'" [ "prepared''" ])
    in
    Lwt_result.return ()

  let join db =
    let check expected =
      let* (Many v) =
        Engine.query db
        @@ Model.select
             ~from:(module T1_T2)
             Sqml.Expression.(
               of_list [ T1_T2.left Model.T1.all; T1_T2.right Model.T2.all ])
      in
      let+ v =
        v
        |> Sequence.map ~f:(fun Hlist.[ t1; t2 ] -> (t1, t2))
        |> Sequence.to_list
      in
      Alcotest.(check ~here:[%here] (list (pair t1 t2)) "selected" expected v)
    and insert_t2 t2 =
      let+ Zero =
        Engine.query db
        @@ Model.insert
             ~into:(module Model.T2)
             Sqml.Expression.(const Model.T2.type_ t2)
      in
      ()
    in
    let* () = populate db in
    let* () = check [] in
    let* () = insert_t2 0 in
    let* () = check [ ((0, "zero", None), 0) ] in
    let* () = insert_t2 0 in
    let* () = insert_t2 2 in
    let* () =
      check
        [
          ((0, "zero", None), 0);
          ((0, "zero", None), 0);
          ((2, "two", Some "2"), 2);
        ]
    in
    let module T1_T2 =
      (val J.left_on Sqml.Expression.(J.left Model.T1.id = J.right Model.T2.id))
    in
    let check expected =
      let* (Many v) =
        Engine.query db
        @@ Model.select
             ~from:(module T1_T2)
             Sqml.Expression.(
               of_list [ T1_T2.left Model.T1.all; T1_T2.right Model.T2.all ])
      in
      let+ v =
        v
        |> Sequence.map ~f:(fun Hlist.[ t1; t2 ] -> (t1, t2))
        |> Sequence.to_list
      in
      let v =
        List.sort v ~compare:(fun ((l, _, _), _) ((r, _, _), _) ->
            Int.compare l r)
      in
      Alcotest.(
        check ~here:[%here] (list (pair t1 (option t2))) "selected" expected v)
    in
    check
      [
        ((0, "zero", None), Some 0);
        ((0, "zero", None), Some 0);
        ((1, "one", Some "1"), None);
        ((2, "two", Some "2"), Some 2);
      ]

  module Versioning = struct
    let stable make () =
      let* _ = make () in
      let* _ = make () in
      Lwt_result.return ()
  end

  let in' db =
    let q =
      let open Sqml.Expression in
      Model.select
        ~from:(module Model.T1)
        ~where:(in' Model.T1.id (const (Array Int) [ 1; 2; 3 ]))
        Model.T1.id
    in
    let* () = populate db in
    let* () =
      let* (Many res) = Engine.query db q in
      let* res = Sequence.to_list res in
      Alcotest.(
        check ~here:[%here] (list int) "selected ids" [ 1; 2 ]
          (List.sort ~compare:Int.compare res))
      |> Lwt_result.return
    in
    Lwt_result.return ()

  let group db =
    let* () =
      populate
        ~t1s:
          [
            (0, "Alice", None);
            (1, "Bob", None);
            (2, "Carol", None);
            (3, "Alice", None);
            (4, "Alice", None);
            (5, "Carol", None);
          ]
        db
    in
    let* () =
      let* (Many res) =
        Engine.query db
        @@ Model.group ~from:(module Model.T1) ~by:Model.T1.Column.name
        @@ fun name ->
        Sqml.Expression.(
          of_list [ name; Group.max Model.T1.Column.id; Group.count ])
      in
      let* res = Sequence.to_list res in
      Alcotest.(
        check ~here:[%here]
          (list (triple string int int))
          "selected names"
          [ ("Alice", 4, 3); ("Bob", 1, 1); ("Carol", 5, 2) ]
          (res
          |> List.sort ~compare:(fun Hlist.(l :: _) Hlist.(r :: _) ->
                 String.compare l r)
          |> List.map ~f:(fun Hlist.[ name; id; count ] -> (name, id, count))))
      |> Lwt_result.return
    in
    Lwt_result.return ()

  let composite db =
    let open Sqml.Expression in
    let* Zero =
      let query =
        Model.insert ~into:(module Model.Configs)
        @@ const Model.Configs.type_
             ( "pipo",
               `String "pipo",
               {
                 Model.Configs.Owner.first_name = "Pipo";
                 last_name = "";
                 age = 0;
               } )
      in
      Engine.query db query
    and* Zero =
      let query =
        let f Sqml.Expression.List.[ user ] =
          Model.insert ~into:(module Model.Configs) user
        in
        Engine.prepare [ Model.Configs.type_ ] f
      in
      Engine.run query db
        [
          ( "molo",
            `String "molo",
            { Model.Configs.Owner.first_name = "Molo"; last_name = ""; age = 1 }
          );
        ]
    in
    let* () =
      let* (Many owners) =
        let query =
          Model.select ~from:(module Model.Configs) Model.Configs.owner
        in
        Engine.query db query
      in
      let+ owners = Sequence.to_list owners in
      Alcotest.(check ~here:[%here] (list (module Model.Configs.Owner)))
        "owners"
        [
          { Model.Configs.Owner.first_name = "Pipo"; last_name = ""; age = 0 };
          { Model.Configs.Owner.first_name = "Molo"; last_name = ""; age = 1 };
        ]
        owners
    in
    let* Zero =
      let query =
        let open Sqml.Expression in
        Model.update
          (module Model.Configs)
          [
            Model.Configs.Column.owner :=
              const
                (to_type Model.Configs.owner)
                { first_name = ""; last_name = ""; age = 0 };
          ]
      in
      Engine.query db query
    in
    let* () =
      let* (Many owners) =
        let query =
          Model.select ~from:(module Model.Configs) Model.Configs.owner
        in
        Engine.query db query
      in
      let+ owners = Sequence.to_list owners in
      Alcotest.(check ~here:[%here] (list (module Model.Configs.Owner)))
        "owners"
        [
          { Model.Configs.Owner.first_name = ""; last_name = ""; age = 0 };
          { Model.Configs.Owner.first_name = ""; last_name = ""; age = 0 };
        ]
        owners
    in
    let* Zero =
      let query =
        let open Sqml.Expression in
        let f List.[ owner ] =
          Model.update
            (module Model.Configs)
            [ Model.Configs.Column.owner := owner ]
        in
        Engine.prepare [ to_type Model.Configs.owner ] f
      in
      Engine.run query db
        [
          {
            Model.Configs.Owner.first_name = "pipo";
            last_name = "molo";
            age = 1;
          };
        ]
    in
    let* () =
      let* (Many owners) =
        let query =
          Model.select ~from:(module Model.Configs) Model.Configs.owner
        in
        Engine.query db query
      in
      let+ owners = Sequence.to_list owners in
      Alcotest.(check ~here:[%here] (list (module Model.Configs.Owner)))
        "owners"
        [
          {
            Model.Configs.Owner.first_name = "pipo";
            last_name = "molo";
            age = 1;
          };
          {
            Model.Configs.Owner.first_name = "pipo";
            last_name = "molo";
            age = 1;
          };
        ]
        owners
    in
    Lwt_result.return ()
end

module Caqti = Sqml_caqti.Make (Model)
module Caqti_test = Test (Caqti)

let () =
  let open Alcotest_lwt in
  let test_case name f =
    let f switch () =
      Lwt.map
        ~f:
          Alcotest.(
            check ~here:[%here] (result unit error) "result" (Result.Ok ()))
        (f switch ())
    in
    test_case name `Slow f
  in
  Lwt_main.run
  @@ run "SQML"
       [
         ( "Queries",
           let test_case name f =
             Alcotest_lwt.test_case name `Quick (fun _ () ->
                 let () = f () in
                 Lwt.return ())
           in
           [ test_case "create" Queries.create ] );
         ( "Caqti-SQLite",
           let make test _ () =
             let open Let.Syntax2 (Lwt_result) in
             let f (name, channel) =
               let* () = Lwt_io.close channel |> Lwt.map ~f:Result.return in
               let* db = Caqti.make ("sqlite3://" ^ name) in
               Caqti.connect db test
             in
             Lwt_io.with_temp_file ~prefix:"sqml-test" f
           and make_db test _ () =
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
             test_case "update" @@ make Caqti_test.update;
             test_case "conflict" @@ make @@ Caqti_test.conflict false;
             test_case "timestamp" @@ make_db Caqti_test.timestamp;
             test_case "json" @@ make Caqti_test.json;
             test_case "versioning-stable"
             @@ make_db Caqti_test.Versioning.stable
             (* test_case "prepared" @@ make Caqti_test.prepared; *);
           ] );
         ( "Caqti-PostgreSQL",
           let make_db db test switch () =
             let pg_host =
               Sys.getenv "PG_HOST" |> Option.value ~default:"localhost"
             in
             let* c =
               Caqti_lwt_unix.connect
                 (Uri.of_string @@ "postgres://postgres@" ^ pg_host)
               |> Lwt.map ~f:convert_error
             in
             let module C = (val c) in
             let* () =
               let () =
                 Lwt_switch.add_hook (Some switch) (fun () ->
                     C.exec
                       ((Caqti_type.unit ->. Caqti_type.unit)
                       @@ Fmt.str "DROP DATABASE %S" db)
                       ()
                     |> Lwt.map ~f:ignore)
               in
               C.exec
                 ((Caqti_type.unit ->. Caqti_type.unit)
                 @@ Fmt.str "CREATE DATABASE %S" db)
                 ()
               |> Lwt.map ~f:convert_error
             in
             let open Let.Syntax (Lwt) in
             let open Let.Syntax2 (Lwt_result) in
             let make () =
               let+ res =
                 Caqti.make ("postgres://postgres@" ^ pg_host ^ "/" ^ db)
               in
               let () =
                 Lwt_switch.add_hook (Some switch) (fun () -> Caqti.close res)
               in
               res
             in
             test make ()
           in
           let make name test switch () =
             let test make () =
               let* db = make () in
               Caqti.connect db test
             in
             make_db name test switch ()
           in
           let test_case name f = test_case name @@ make name f
           and test_case_db name f = test_case name @@ make_db name f in
           [
             test_case "select" Caqti_test.select;
             test_case "insert" Caqti_test.insert;
             test_case "delete" Caqti_test.delete;
             test_case "delete_returning" Caqti_test.delete_returning;
             test_case "update" Caqti_test.update;
             test_case "update_from" Caqti_test.update_from;
             test_case_db "timestamp" Caqti_test.timestamp;
             test_case "group" Caqti_test.group;
             test_case "is_null" Caqti_test.is_null;
             test_case "json" Caqti_test.json;
             test_case "in" Caqti_test.in';
             test_case "conflict" @@ Caqti_test.conflict true;
             test_case_db "versioning-stable" Caqti_test.Versioning.stable;
             test_case "prepared" Caqti_test.prepared;
             test_case "join" Caqti_test.join;
             test_case "composite" Caqti_test.composite;
           ] );
       ]
