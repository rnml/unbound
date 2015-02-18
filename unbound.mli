open Core.Std

module Nat : sig
  type t
  val zero : t
end

module Name : sig
  type t
  include Identifiable with type t := t
  val freshen : t -> t
end

module Pattern_tc : sig
  type 'a t = {
    binders  : 'a -> Name.t list;
    freshen  : 'a -> 'a;
    close    : 'p. 'p t -> Nat.t -> 'p -> 'a -> 'a;
    open_    : 'p. 'p t -> Nat.t -> 'p -> 'a -> 'a;
    compare  : 'a -> 'a -> int;
    fv       : 'a -> Name.Set.t;
  }
  val equal : 'a t -> 'a -> 'a -> bool
  val pair  : 'a t -> 'b t -> ('a * 'b) t
end

module Term_tc : sig
  type 'a t = {
    close   : 'p. 'p Pattern_tc.t -> Nat.t -> 'p -> 'a -> 'a;
    open_   : 'p. 'p Pattern_tc.t -> Nat.t -> 'p -> 'a -> 'a;
    compare : 'a -> 'a -> int;
    fv      : 'a -> Name.Set.t;
  }
  val equal : 'a t -> 'a -> 'a -> bool
  val pair  : 'a t -> 'b t -> ('a * 'b) t
  val map   : 'a t -> (_, 'a, _) Map.t t
  val const : cmp:('a -> 'a -> int) -> 'a t
end

module Make_name_type (X : sig val module_name : string end) : sig
  include Identifiable
  val match_ : Name.t -> t option
  val ptc : t Pattern_tc.t
  val tc  : t Term_tc.t
end

module Bind : sig
  type ('p, 't) t with sexp, compare
  (* TODO: flip argument order for [create] and [expose] so that Pattern comes before
     Term *)
  val create : 'a Term_tc.t -> 'p Pattern_tc.t -> 'p -> 'a -> ('p, 'a) t
  val expose : 'a Term_tc.t -> 'p Pattern_tc.t -> ('p, 'a) t -> 'p * 'a
  val tc : 'a Pattern_tc.t -> 'b Term_tc.t -> ('a, 'b) t Term_tc.t
end

module Rebind : sig
  type ('p1, 'p2) t with sexp, compare
  val create : 'a Pattern_tc.t -> 'p Pattern_tc.t -> 'p -> 'a -> ('p, 'a) t
  val expose : 'a Pattern_tc.t -> 'p Pattern_tc.t -> ('p, 'a) t -> 'p * 'a
  val tc : 'a Pattern_tc.t -> 'b Pattern_tc.t -> ('a, 'b) t Pattern_tc.t
end

module Embed : sig
  type 'p t with sexp, compare
  val create : 'a Term_tc.t -> 'a -> 'a t
  val expose : 'a Term_tc.t -> 'a t -> 'a
  val tc : 'a Term_tc.t -> 'a t Pattern_tc.t
end

module Rec : sig
  type 'p t with sexp, compare
  val create : 'a Pattern_tc.t -> 'a -> 'a t
  val expose : 'a Pattern_tc.t -> 'a t -> 'a
  val tc : 'a Pattern_tc.t -> 'a t Pattern_tc.t
end
