open Core.Std
open Unbound_lib.Unbound

module Label : Identifiable

(* type variable *)
module Var : Identifiable

(* type expressions *)
module Expr : sig
  type t

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val fv      : t -> Var.Set.t

  module Shape : sig
    type 'a t =
      | Var   of Var.t
      | Tuple of 'a * 'a * 'a list
      | Map   of 'a * Label.t
      | App   of Var.t * 'a list

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  val open_ : t -> t Shape.t
  val close : t Shape.t -> t
end

module Decl : sig
  type t

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val fv      : t -> Var.Set.t

  module Shape : sig
    type 'a t =
      | Variant of Expr.t Label.Map.t
      | Record  of Expr.t Label.Map.t

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  val open_ : t -> t Shape.t
  val close : t Shape.t -> t
end
