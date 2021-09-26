(** Any object that can be selected from *)

type ast =
  | Selectable of {
      name : string;
      alias : string option;
    }
  | Join of {
      left : ast;
      right : ast;
      on : Expression.ast option;
    }

module type S = sig
  type model

  (** Type identifier of columns that can be selected from this *)
  type tag

  val to_ast : unit -> ast
end

type 'tag t = (module S with type tag = 'tag)
