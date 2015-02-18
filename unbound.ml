open Core.Std

module Nat : sig
  type t with sexp, compare, bin_io
  val equal : t -> t -> bool
  val zero : t
  val succ : t -> t
  val list_nth : 'a list -> t -> 'a option
  val list_findi : 'a list -> equal:('a -> 'a -> bool) -> 'a -> t option
end = struct
  type t = int with sexp, compare, bin_io
  let equal t1 t2 = compare t1 t2 = 0
  let zero = 0
  let succ i = i + 1
  let list_nth xs i = List.nth xs i
  let list_findi xs ~equal x = Option.map ~f:fst @@ List.findi xs ~f:(fun _ y -> equal x y)
end

module Name_value : sig
  include Identifiable
  val freshen : t -> t
end = struct

  module T = struct
    type t = {
      name : String_id.t;
      id : int option;
    } with bin_io, compare

    let hash (t : t) = Hashtbl.hash t

    let to_string t =
      match t.id with
      | None   -> (t.name :> string)
      | Some n -> (t.name :> string) ^ "_" ^ Int.to_string n

    let of_string s =
      match String.rsplit2 s ~on:'_' with
      | None -> {name = String_id.of_string s; id = None}
      | Some (name, id) ->
        match Option.try_with (fun () -> Int.of_string id) with
        | None -> {name = String_id.of_string name; id = None}
        | id   -> {name = String_id.of_string name; id}

    let freshen {name; id} =
      match id with
      | None   -> {name; id = Some 1}
      | Some n -> {name; id = Some (n + 1)}
  end

  include T
  include Identifiable.Make (struct
    let module_name = "Name_value"
    include T
    include Sexpable.Of_stringable (T)
  end)
end

module Name_type = Unique_id.Int (struct end)

(* universal names *)
module Name = struct
  type nonrec t = Name_type.t * Name_value.t with sexp, bin_io, compare
  include Identifiable.Make (struct
    type nonrec t = t with sexp, bin_io, compare
    let module_name = "Name"
    let to_string t = sexp_of_t t |> Sexp.to_string
    let of_string s = t_of_sexp @@ Sexp.of_string s
    let hash (t : t) = Hashtbl.hash t
  end)
  let freshen (k, x) = (k, Name_value.freshen x)
end

module Pattern_tc = struct
  type 'a t = {
    binders  : 'a -> Name.t list;
    freshen  : 'a -> 'a; (* effectful *)
    close    : 'p. 'p t -> Nat.t -> 'p -> 'a -> 'a;
    open_    : 'p. 'p t -> Nat.t -> 'p -> 'a -> 'a;
    compare  : 'a -> 'a -> int; (* expose *)
    fv       : 'a -> Name.Set.t; (* expose *)
  }

  let nth t a i = Nat.list_nth (t.binders a) i

  let find t a x = Nat.list_findi (t.binders a) ~equal:Name.equal x

  let equal t a a' = t.compare a a' = 0

  let pair : type a b. a t -> b t -> (a * b) t = fun ta tb -> {
      binders = (fun (a, b) -> ta.binders a @ tb.binders b);
      freshen = (fun (a, b) -> (ta.freshen a, tb.freshen b));
      close   = (fun t l p (a, b) -> (ta.close t l p a, tb.close t l p b));
      open_   = (fun t l p (a, b) -> (ta.open_ t l p a, tb.open_ t l p b));
      compare = (fun (a1, b1) (a2, b2) ->
        let acmp = ta.compare a1 a2 in
        if acmp <> 0 then acmp else tb.compare b1 b2
      );
      fv = (fun (a, b) -> Set.union (ta.fv a) (tb.fv b));
    }
end

module Term_tc = struct
  type 'a t = {
    close   : 'p. 'p Pattern_tc.t -> Nat.t -> 'p -> 'a -> 'a;
    open_   : 'p. 'p Pattern_tc.t -> Nat.t -> 'p -> 'a -> 'a;
    compare : 'a -> 'a -> int;
    fv      : 'a -> Name.Set.t;
  }

  let equal t a a' = t.compare a a' = 0

  let pair : type a b. a t -> b t -> (a * b) t = fun ta tb -> {
      close   = (fun t l p (a, b) -> (ta.close t l p a, tb.close t l p b));
      open_   = (fun t l p (a, b) -> (ta.open_ t l p a, tb.open_ t l p b));
      compare = (fun (a1, b1) (a2, b2) ->
        let acmp = ta.compare a1 a2 in
        if acmp <> 0 then acmp else tb.compare b1 b2
      );
      fv = (fun (a, b) -> Set.union (ta.fv a) (tb.fv b));
    }

  let map : type a k cmp. a t -> (k, a, cmp) Map.t t = fun t -> {
      close   = (fun pat_tc i pat map -> Map.map ~f:(t.close pat_tc i pat) map);
      open_   = (fun pat_tc i pat map -> Map.map ~f:(t.open_ pat_tc i pat) map);
      compare = Map.compare_direct t.compare;
      fv = (fun map ->
        Map.fold map ~init:Name.Set.empty ~f:(fun ~key:_ ~data acc ->
          Set.union acc (t.fv data)));
    }

  let const : type a. cmp:(a -> a -> int) -> a t = fun ~cmp -> {
      close   = (fun _ _ _ k -> k);
      open_   = (fun _ _ _ k -> k);
      compare = cmp;
      fv = (fun _ -> Name.Set.empty);
    }
end

module Make_name_type (X : sig val module_name : string end) : sig
  include Identifiable
  val of_name : Name_value.t -> t
  val to_name : t -> Name_value.t
  val match_ : Name.t -> t option
  val ptc : t Pattern_tc.t
  val tc  : t Term_tc.t
end = struct

  module X = struct
    include X
    include Name_value
    let uid = Name_type.create ()
    let pattern x = if Name_type.equal uid (fst x) then Some (snd x) else None
    let constructor x = (uid, x)
  end

  module Index = struct
    type t = {
      j : Nat.t; (* pattern offset *)
      k : Nat.t; (* intra-pattern variable offset *)
    } with bin_io, compare, sexp
  end

  module T = struct
    type t =
      | Free of X.t
      | Bound of Index.t
    with bin_io, compare

    let hash (t : t) = Hashtbl.hash t

    let of_name x = Free x

    let to_name = function
      | Free x -> x
      | Bound _ -> assert false

    let of_string x = of_name @@ X.of_string x
    let to_string t = to_name t |> X.to_string
  end
  include T
  include Identifiable.Make (struct
    let module_name = X.module_name
    include T
    include Sexpable.Of_stringable (T)
  end)

  let match_ x = Option.map (X.pattern x) ~f:(fun x -> Free x)

  let ptc : t Pattern_tc.t = {
    binders = (fun x -> [X.constructor (to_name x)]);
    freshen = (function
      | Free x -> Free (X.freshen x)
      | Bound b -> Bound b
    );
    close = (fun _ _ _ x -> x);
    open_ = (fun _ _ _ x -> x);
    compare;
    fv = (fun x -> Name.Set.singleton (X.constructor (to_name x)));
  }

  let tc : t Term_tc.t = {
    close =
      (fun ptc l p t ->
         match t with
         | Bound b -> Bound b
         | Free x ->
           match Pattern_tc.find ptc p (X.constructor x) with
           | Some i -> Bound {Index.j = l; k = i}
           | None -> t
      );
    open_ =
      (fun ptc l p t ->
         match t with
         | Free x -> Free x
         | Bound {j; k} ->
           if Nat.equal j l then
             match Pattern_tc.nth ptc p k with
             | None -> t
             | Some name ->
               match X.pattern name with
               | Some x -> Free x
               | None -> assert false
           else
             t
      );
    compare;
    fv =
      (function
        | Free x -> Name.Set.singleton (X.constructor x)
        | Bound _ -> Name.Set.empty
      );
  }
end

module Bind = struct
  type ('p, 't) t = 'p * 't with sexp, compare

  let tc (atc : _ Pattern_tc.t) (btc : _ Term_tc.t) : _ Term_tc.t = {
    close = (fun tc l p (a, b) ->
      (atc.close tc l p a, btc.close tc (Nat.succ l) p b));
    open_ = (fun tc l p (a, b) ->
      (atc.open_ tc l p a, btc.open_ tc (Nat.succ l) p b));
    compare = (fun t1 t2 ->
      Comparable.lexicographic [
        (fun (a1, _) (a2, _) -> atc.compare a1 a2);
        (fun (_, b1) (_, b2) -> btc.compare b1 b2);
      ] t1 t2
    );
    fv = (fun (a, b) -> Set.union (atc.fv a) (btc.fv b));
  }

  let create (t : _ Term_tc.t) (ptc : _ Pattern_tc.t) p a =
    (p, t.close ptc Nat.zero p a)

  let expose : type p a. a Term_tc.t -> p Pattern_tc.t -> (p, a) t -> p * a =
    fun t ptc (p, a) ->
      let p = ptc.freshen p in
      (p, t.open_ ptc Nat.zero p a)
end

module Rebind = struct
  type ('p1, 'p2) t = 'p1 * 'p2 with compare, sexp

  let tc (atc : _ Pattern_tc.t) (btc : _ Pattern_tc.t) : _ Pattern_tc.t = {
    binders = (fun (a, b) -> atc.binders a @ btc.binders b);
    freshen = (fun (a, b) -> (atc.freshen a, btc.freshen b));
    close = (fun tc l p (a, b) ->
      (atc.close tc l p a, btc.close tc (Nat.succ l) p b));
    open_ = (fun tc l p (a, b) ->
      (atc.open_ tc l p a, btc.open_ tc (Nat.succ l) p b));
    compare = (fun t1 t2 ->
      Comparable.lexicographic [
        (fun (a1, _) (a2, _) -> atc.compare a1 a2);
        (fun (_, b1) (_, b2) -> btc.compare b1 b2);
      ] t1 t2
    );
    fv = (fun (a, b) -> Set.union (atc.fv a) (btc.fv b));
  }

  let create (atc : _ Pattern_tc.t) ptc  p  a  = (p, atc.close ptc Nat.zero p a)
  let expose (atc : _ Pattern_tc.t) ptc (p, a) = (p, atc.open_ ptc Nat.zero p a)
end

module Embed = struct
  type 'p t = 'p with compare, sexp

  let tc (atc : _ Term_tc.t) : _ Pattern_tc.t = {
    binders = (fun _ -> []);
    freshen = (fun a -> a);
    close = (fun tc l p a -> atc.close tc l p a);
    open_ = (fun tc l p a -> atc.open_ tc l p a);
    compare = (fun a1 a2 -> atc.compare a1 a2);
    fv = (fun a -> atc.fv a);
  }

  let create _ a = a
  let expose _ a = a
end

module Rec = struct
  type 'p t = 'p with compare, sexp

  let tc (atc : _ Pattern_tc.t) : _ Pattern_tc.t = {
    binders = (fun a -> atc.binders a);
    freshen = (fun a -> atc.freshen a);
    close = (fun tc l p a -> atc.close tc (Nat.succ l) p a);
    open_ = (fun tc l p a -> atc.open_ tc (Nat.succ l) p a);
    compare = (fun a1 a2 -> atc.compare a1 a2);
    fv = (fun a -> atc.fv a);
  }

  let create (t : _ Pattern_tc.t) a = t.close t Nat.zero a a
  let expose (t : _ Pattern_tc.t) a = t.open_ t Nat.zero a a
end
