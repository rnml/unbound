open Core.Std
open Unbound_lib.Unbound

module Name = Make_name_type (struct let module_name = "Pts.Name" end)

type t =
  | Type
  | All  of (Name.t * t Embed.t, t) Bind.t
  | Lam  of (Name.t * t Embed.t, t) Bind.t
  | App  of (t * t)
  | Name of Name.t
with sexp

let rec tc : t Term_tc.t = {
  close = (fun ptc l p t ->
    match t with
    | Name x -> let tc = Lazy.force name_tc in Name (tc.close ptc l p x)
    | All  x -> let tc = Lazy.force bind_tc in All  (tc.close ptc l p x)
    | Lam  x -> let tc = Lazy.force bind_tc in Lam  (tc.close ptc l p x)
    | App  x -> let tc = Lazy.force pair_tc in App  (tc.close ptc l p x)
    | Type   -> Type
  );
  open_ = (fun ptc l p t ->
    match t with
    | Name x -> let tc = Lazy.force name_tc in Name (tc.open_ ptc l p x)
    | All  x -> let tc = Lazy.force bind_tc in All  (tc.open_ ptc l p x)
    | Lam  x -> let tc = Lazy.force bind_tc in Lam  (tc.open_ ptc l p x)
    | App  x -> let tc = Lazy.force pair_tc in App  (tc.open_ ptc l p x)
    | Type   -> Type
  );
  compare;
  fv = (function
    | Name x -> let tc = Lazy.force name_tc in tc.fv x
    | All  x -> let tc = Lazy.force bind_tc in tc.fv x
    | Lam  x -> let tc = Lazy.force bind_tc in tc.fv x
    | App  x -> let tc = Lazy.force pair_tc in tc.fv x
    | Type   -> Unbound_lib.Unbound.Name.Set.empty
  );
}

and name_tc : Name.t Term_tc.t Lazy.t =
  lazy Name.tc

and bind_tc : (Name.t * t Embed.t, t) Bind.t Term_tc.t Lazy.t =
  lazy (Bind.tc (Pattern_tc.pair Name.ptc (Embed.tc tc)) tc)

and pair_tc : (t * t) Term_tc.t Lazy.t =
  lazy (Term_tc.pair tc tc)

let compare = tc.compare
let equal   = Term_tc.equal tc

let fv t =
  tc.fv t
  |> Set.to_list
  |> List.filter_map ~f:Name.match_
  |> Name.Set.of_list

module Shape = struct
  type 'a t =
    | Type
    | All  of Name.t * 'a * 'a
    | Lam  of Name.t * 'a * 'a
    | App  of 'a * 'a
    | Name of Name.t

  let rec map t ~f =
    match t with
    | Type -> Type
    | All  (x, a, b) -> All (x, f a, f b)
    | Lam  (x, a, b) -> Lam (x, f a, f b)
    | App  (a, b) -> App (f a, f b)
    | Name x -> Name x
end

let open_ : t -> t Shape.t = function
  | Type -> Type
  | Name x -> Name x
  | App (a, b) -> App (a, b)
  | All bind ->
    let ((x, arg_type), body) =
      Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc tc)) bind
    in
    let arg_type = Embed.expose tc arg_type in
    All (x, arg_type, body)
  | Lam bind ->
    let ((x, arg_type), body) =
      Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc tc)) bind
    in
    let arg_type = Embed.expose tc arg_type in
    Lam (x, arg_type, body)

let close : t Shape.t -> t = function
  | Type -> Type
  | Name x -> Name x
  | App (a, b) -> App (a, b)
  | All (x, arg_type, body) ->
    let arg_type = Embed.create tc arg_type in
    let bind =
      Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc tc)) (x, arg_type) body
    in
    All bind
  | Lam (x, arg_type, body) ->
    let arg_type = Embed.create tc arg_type in
    let bind =
      Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc tc)) (x, arg_type) body
    in
    Lam bind
