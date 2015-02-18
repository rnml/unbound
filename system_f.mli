open Core.Std

module Kind : sig
  type t =
    | Type
    | Fun of t * t
  with sexp

  val equal : t -> t -> bool
end

module Type : sig

  module Name : Identifiable

  type t with sexp

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val fv      : t -> Name.Set.t

  module Shape : sig
    type 'a t =
      | Name of Name.t
      | Fun  of 'a * 'a
      | All  of Name.t * Kind.t * 'a
      | Lam  of Name.t * Kind.t * 'a
      | App  of 'a * 'a
    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  val open_ : t -> t Shape.t
  val close : t Shape.t -> t
end

module Term : sig

  module Name : Identifiable

  type t with sexp

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val ftv     : t -> Type.Name.Set.t
  val fv      : t -> Name.Set.t

  module Shape : sig
    type 'a t =
      | Name   of Name.t
      | Lam    of Name.t * Type.t * 'a
      | App    of 'a * 'a
      | Ty_lam of Type.Name.t * Kind.t * 'a
      | Ty_app of 'a * Type.t
    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  val open_ : t -> t Shape.t
  val close : t Shape.t -> t
end
