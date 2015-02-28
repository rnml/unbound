open Core.Std
open Unbound_lib.Unbound

module Label : Identifiable

(* type variable *)
module Var : Identifiable

(* type expressions *)
module Type : sig
  type t

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val fv      : t -> Var.Set.t

  module Shape : sig
    type 'a t =
      | Var   of Var.t
      | App   of Var.t * 'a list

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  val open_ : t -> t Shape.t
  val close : t Shape.t -> t
end

(* module Decl : sig
 *   type t
 *
 *   val compare : t -> t -> int
 *   val equal   : t -> t -> bool
 *   val fv      : t -> Var.Set.t
 *
 *   module Shape : sig
 *     type 'a t = Type.t Label.Map.t
 *
 *     val map : 'a t -> f:('a -> 'b) -> 'b t
 *   end
 *
 *   val open_ : t -> t Shape.t
 *   val close : t Shape.t -> t
 * end *)
