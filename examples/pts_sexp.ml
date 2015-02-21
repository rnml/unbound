open Core.Std
open Unbound_lib.Unbound

type t = Pts.t

let rec sexp_of_t t : Sexp.t =
  match Pts.open_ t with
  | Type -> Atom "type"
  | All (x, a, b) ->
    let rec gather args b =
      match Pts.open_ b with
      | All (x, a, b) -> gather ((x, a) :: args) b
      | _ -> (List.rev args, b)
    in
    let (args, body) = gather [(x, a)] b in
    let args = List.map args ~f:<:sexp_of< Pts.Name.t * t >> in
    let body = sexp_of_t body in
    Sexp.List (Sexp.Atom "all" :: args @ [body])
  | Lam (x, a, b) ->
    let rec gather args b =
      match Pts.open_ b with
      | Lam (x, a, b) -> gather ((x, a) :: args) b
      | _ -> (List.rev args, b)
    in
    let (args, body) = gather [(x, a)] b in
    let args = List.map args ~f:<:sexp_of< Pts.Name.t * t >> in
    let body = sexp_of_t body in
    Sexp.List (Sexp.Atom "lam" :: args @ [body])
  | App (a, b) ->
    let rec gather fn args =
      match Pts.open_ b with
      | App (a, b) -> gather a (b :: args)
      | _ -> (fn, args)
    in
    let (head, args) = gather a [b] in
    let head = sexp_of_t head in
    let args = List.map args ~f:<:sexp_of< t >> in
    Sexp.List (head :: args)
  | Name x ->
    <:sexp_of< Pts.Name.t >> x

let rec t_of_sexp sexp : Pts.t =
  begin
    match (sexp : Sexp.t) with
    | Sexp.Atom "type" -> Pts.close Type
    | Sexp.Atom _ as x ->
      Pts.close (Name (<:of_sexp< Pts.Name.t >> x))
    | Sexp.List (Sexp.Atom "all" :: rest) ->
      begin
        match List.rev rest with
        | [] -> of_sexp_error "empty all" sexp
        | body :: rev_args ->
          let args = List.rev rev_args |> List.map ~f:<:of_sexp< Pts.Name.t * t >> in
          let body = t_of_sexp body in
          List.fold_right args ~init:body ~f:(fun (x, a) body ->
            Pts.close (All (x, a, body)))
      end
    | Sexp.List (Sexp.Atom "lam" :: rest) ->
      begin
        match List.rev rest with
        | [] -> of_sexp_error "empty lam" sexp
        | body :: rev_args ->
          let args = List.rev rev_args |> List.map ~f:<:of_sexp< Pts.Name.t * t >> in
          let body = t_of_sexp body in
          List.fold_right args ~init:body ~f:(fun (x, a) body ->
            Pts.close (Lam (x, a, body)))
      end
    | Sexp.List [] -> of_sexp_error "empty list" sexp
    | Sexp.List (head :: args) ->
      let head = t_of_sexp head in
      let args = List.map ~f:t_of_sexp args in
      List.fold ~init:head args ~f:(fun head arg ->
        Pts.close (App (head, arg)))
  end
