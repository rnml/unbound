open Core.Std

open Unbound

module Kind = struct
  type t =
    | Type
    | Fun of t * t
  with sexp, compare

  let tc = Term_tc.const ~cmp:compare

  let compare = tc.compare
  let equal   = Term_tc.equal tc
end

module Type = struct

  module Name = Make_name_type (struct let module_name = "System_f.Type.Name" end)

  type t =
    | Name  of Name.t
    | Fun   of (t * t)
    | All   of (Name.t * Kind.t Embed.t, t) Bind.t
    | Lam   of (Name.t * Kind.t Embed.t, t) Bind.t
    | App   of (t * t)
  with sexp

  let rec tc : t Term_tc.t = {
    close = (fun ptc l p t ->
      match t with
      | Name x -> let tc = Lazy.force name_tc in Name (tc.close ptc l p x)
      | Fun  x -> let tc = Lazy.force pair_tc in Fun  (tc.close ptc l p x)
      | All  x -> let tc = Lazy.force bind_tc in All  (tc.close ptc l p x)
      | Lam  x -> let tc = Lazy.force bind_tc in Lam  (tc.close ptc l p x)
      | App  x -> let tc = Lazy.force pair_tc in App  (tc.close ptc l p x)
    );
    open_ = (fun ptc l p t ->
      match t with
      | Name x -> let tc = Lazy.force name_tc in Name (tc.open_ ptc l p x)
      | Fun  x -> let tc = Lazy.force pair_tc in Fun  (tc.open_ ptc l p x)
      | All  x -> let tc = Lazy.force bind_tc in All  (tc.open_ ptc l p x)
      | Lam  x -> let tc = Lazy.force bind_tc in Lam  (tc.open_ ptc l p x)
      | App  x -> let tc = Lazy.force pair_tc in App  (tc.open_ ptc l p x)
    );
    compare;
    fv = (function
      | Name x -> let tc = Lazy.force name_tc in tc.fv x
      | Fun  x -> let tc = Lazy.force pair_tc in tc.fv x
      | All  x -> let tc = Lazy.force bind_tc in tc.fv x
      | Lam  x -> let tc = Lazy.force bind_tc in tc.fv x
      | App  x -> let tc = Lazy.force pair_tc in tc.fv x
    );
  }

  and name_tc : Name.t Term_tc.t Lazy.t =
    lazy Name.tc

  and bind_tc : (Name.t * Kind.t Embed.t, t) Bind.t Term_tc.t Lazy.t =
    lazy (Bind.tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) tc)

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
      | Name of Name.t
      | Fun  of 'a * 'a
      | All  of Name.t * Kind.t * 'a
      | Lam  of Name.t * Kind.t * 'a
      | App  of 'a * 'a

    let rec map t ~f =
      match t with
      | Name x -> Name x
      | Fun (a, b) -> Fun (f a, f b)
      | All (x, a, b) -> All (x, a, f b)
      | Lam (x, a, b) -> Lam (x, a, f b)
      | App (a, b) -> App (f a, f b)
  end

  let open_ : t -> t Shape.t = function
    | Name x -> Name x
    | App (a, b) -> App (a, b)
    | Fun (a, b) -> Fun (a, b)
    | All bind ->
      let ((x, arg_type), body) =
        Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) bind
      in
      let arg_type = Embed.expose Kind.tc arg_type in
      All (x, arg_type, body)
    | Lam bind ->
      let ((x, arg_type), body) =
        Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) bind
      in
      let arg_type = Embed.expose Kind.tc arg_type in
      Lam (x, arg_type, body)

  let close : t Shape.t -> t = function
    | Name x -> Name x
    | App (a, b) -> App (a, b)
    | Fun (a, b) -> Fun (a, b)
    | All (x, arg_type, body) ->
      let arg_type = Embed.create Kind.tc arg_type in
      let bind =
        Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) (x, arg_type) body
      in
      All bind
    | Lam (x, arg_type, body) ->
      let arg_type = Embed.create Kind.tc arg_type in
      let bind =
        Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) (x, arg_type) body
      in
      Lam bind
end

module Term = struct

  module Name = Make_name_type (struct let module_name = "System_f.Term.Name" end)

  type t =
    | Name   of Name.t
    | Lam    of (Name.t * Type.t Embed.t, t) Bind.t
    | App    of (t * t)
    | Ty_lam of (Type.Name.t * Kind.t Embed.t, t) Bind.t
    | Ty_app of (t * Type.t)
  with sexp

  let rec tc : t Term_tc.t = {
    close = (fun ptc l p t ->
      match t with
      | Name   x -> let tc = Lazy.force   name_tc in Name   (tc.close ptc l p x)
      | Lam    x -> let tc = Lazy.force    lam_tc in Lam    (tc.close ptc l p x)
      | App    x -> let tc = Lazy.force    app_tc in App    (tc.close ptc l p x)
      | Ty_lam x -> let tc = Lazy.force ty_lam_tc in Ty_lam (tc.close ptc l p x)
      | Ty_app x -> let tc = Lazy.force ty_app_tc in Ty_app (tc.close ptc l p x)
    );
    open_ = (fun ptc l p t ->
      match t with
      | Name   x -> let tc = Lazy.force   name_tc in Name   (tc.open_ ptc l p x)
      | Lam    x -> let tc = Lazy.force    lam_tc in Lam    (tc.open_ ptc l p x)
      | App    x -> let tc = Lazy.force    app_tc in App    (tc.open_ ptc l p x)
      | Ty_lam x -> let tc = Lazy.force ty_lam_tc in Ty_lam (tc.open_ ptc l p x)
      | Ty_app x -> let tc = Lazy.force ty_app_tc in Ty_app (tc.open_ ptc l p x)
    );
    compare;
    fv = (function
      | Name   x -> let tc = Lazy.force   name_tc in tc.fv x
      | Lam    x -> let tc = Lazy.force    lam_tc in tc.fv x
      | App    x -> let tc = Lazy.force    app_tc in tc.fv x
      | Ty_lam x -> let tc = Lazy.force ty_lam_tc in tc.fv x
      | Ty_app x -> let tc = Lazy.force ty_app_tc in tc.fv x
    );
  }

  and name_tc : Name.t Term_tc.t Lazy.t =
    lazy Name.tc

  and lam_tc : (Name.t * Type.t Embed.t, t) Bind.t Term_tc.t Lazy.t =
    lazy (Bind.tc (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) tc)

  and app_tc : (t * t) Term_tc.t Lazy.t =
    lazy (Term_tc.pair tc tc)

  and ty_lam_tc : (Type.Name.t * Kind.t Embed.t, t) Bind.t Term_tc.t Lazy.t =
    lazy (Bind.tc (Pattern_tc.pair Type.Name.ptc (Embed.tc Kind.tc)) tc)

  and ty_app_tc : (t * Type.t) Term_tc.t Lazy.t =
    lazy (Term_tc.pair tc Type.tc)

  let compare = tc.compare
  let equal   = Term_tc.equal tc

  let ftv t =
    tc.fv t
    |> Set.to_list
    |> List.filter_map ~f:Type.Name.match_
    |> Type.Name.Set.of_list

  let fv t =
    tc.fv t
    |> Set.to_list
    |> List.filter_map ~f:Name.match_
    |> Name.Set.of_list

  module Shape = struct
    type 'a t =
      | Name   of Name.t
      | Lam    of Name.t * Type.t * 'a
      | App    of 'a * 'a
      | Ty_lam of Type.Name.t * Kind.t * 'a
      | Ty_app of 'a * Type.t

    let rec map t ~f =
      match t with
      | Name x -> Name x
      | Lam (x, a, b) -> Lam (x, a, f b)
      | App (a, b) -> App (f a, f b)
      | Ty_lam (x, a, b) -> Ty_lam (x, a, f b)
      | Ty_app (a, b) -> Ty_app (f a, b)
  end

  let open_ : t -> t Shape.t = function
    | Name x -> Name x
    | App (a, b) -> App (a, b)
    | Lam bind ->
      let ((x, arg_type), body) =
        Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) bind
      in
      let arg_type = Embed.expose Type.tc arg_type in
      Lam (x, arg_type, body)
    | Ty_app (a, b) -> Ty_app (a, b)
    | Ty_lam bind ->
      let ((x, arg_type), body) =
        Bind.expose tc (Pattern_tc.pair Type.Name.ptc (Embed.tc Kind.tc)) bind
      in
      let arg_type = Embed.expose Kind.tc arg_type in
      Ty_lam (x, arg_type, body)

  let close : t Shape.t -> t = function
    | Name x -> Name x
    | App (a, b) -> App (a, b)
    | Lam (x, arg_type, body) ->
      let arg_type = Embed.create Type.tc arg_type in
      let bind =
        Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) (x, arg_type) body
      in
      Lam bind
    | Ty_app (a, b) -> Ty_app (a, b)
    | Ty_lam (x, arg_type, body) ->
      let arg_type = Embed.create Kind.tc arg_type in
      let bind =
        Bind.create tc (Pattern_tc.pair Type.Name.ptc (Embed.tc Kind.tc)) (x, arg_type) body
      in
      Ty_lam bind
end
