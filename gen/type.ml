open Core.Std
open Unbound_lib.Unbound

module Label : Identifiable = String_id

(* type variable *)
module Var = Make_name_type (struct let module_name = "Var" end)

module Type = struct

  type t =
    | Var of (Var.t)
    | App of (Var.t * t list)
  with sexp

  let rec tc : t Term_tc.t = {
    close = (fun ptc l p t ->
      match t with
      | Var x -> let tc = Lazy.force var_tc in Var (tc.close ptc l p x)
      | App x -> let tc = Lazy.force app_tc in App (tc.close ptc l p x)
    );
    open_ = (fun ptc l p t ->
      match t with
      | Var x -> let tc = Lazy.force var_tc in Var (tc.open_ ptc l p x)
      | App x -> let tc = Lazy.force app_tc in App (tc.open_ ptc l p x)
    );
    compare;
    fv = (function
      | Var x -> let tc = Lazy.force var_tc in tc.fv x
      | App x -> let tc = Lazy.force app_tc in tc.fv x
    );
  }

  and var_tc : Var.t Term_tc.t Lazy.t =
    lazy Var.tc

  and app_tc : (Var.t * t list) Term_tc.t Lazy.t =
    lazy ((Term_tc.pair Var.tc (Term_tc.list tc)))

  let compare = tc.compare
  let equal   = Term_tc.equal tc

  let fv t =
    tc.fv t
    |> Set.to_list
    |> List.filter_map ~f:Var.match_
    |> Var.Set.of_list

  module Shape = struct
    type 'a t =
      | Var of Var.t
      | App of Var.t * 'a list

    let rec map t ~f =
      match t with
      | Var x -> Var x
      | App  (a, b) -> App (a, List.map ~f b)
  end

  let open_ : t -> t Shape.t = function
    | Var x -> Var x
    | App (a, b) -> App (a, b)

  let close : t Shape.t -> t = function
    | Var x -> Var x
    | App (a, b) -> App (a, b)

end
