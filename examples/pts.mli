open Core.Std
open Unbound_lib.Unbound

module Name : Identifiable

type t

val compare : t -> t -> int
val equal   : t -> t -> bool
val fv      : t -> Name.Set.t

module Shape : sig
  type 'a t =
    | Type
    | All  of Name.t * 'a * 'a
    | Lam  of Name.t * 'a * 'a
    | App  of 'a * 'a
    | Name of Name.t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

val open_ : t -> t Shape.t
val close : t Shape.t -> t
