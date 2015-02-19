open Core.Std
open Unbound_lib.Unbound

(* let slice tm =
 *   match Pts.open_ tm with
 *   | Name x ->
 *   | Lam  of Name.t * 'a * 'a
 *   | App  of 'a * 'a
 *   | All  of Name.t * 'a * 'a
 *   | Type
 *
 * let rec free_theorem tm =
 *   match Pts.open_ tm with
 *   | Name x ->
 *   | Lam  of Name.t * 'a * 'a
 *   | App  of 'a * 'a
 *   | All  of Name.t * 'a * 'a
 *   | Type *)



(*
F A = forall r. (A -> r) -> r

i : forall a:*. a -> F a
i [a] x [r] k = k x

j : forall a:*. F a -> a
j [a] m = m [a] (fun x -> x)

forall a:*. forall x:a. j [a] (i [a] x) = x
   j [a] (i [a] x)
     = j [a] (\[r] k. k x)
     = (\[r] k. k x) [a] (fun x -> x)
     = (\k. k x) (fun x -> x)
     = (fun x -> x) x
     = x

forall a:*. forall m : F a. i [a] (j [a] m) = m

j+ : forall a1 : *.
     forall a2 : *.
     forall a+ : a1 -> a2 -> *.
     forall f1 : F a1.
     forall f2 : F a2.
     ( forall r1 : *.
       forall r2 : *.
       forall r+ : r1 -> r2 -> *.
       forall k1 : a1 -> r1.
       forall k2 : a2 -> r2.
       ( forall x1:a1.
         forall x2:a2.
         a+ x1 x2
         -> r+ (k1 x1) (k2 x2)
       )
       -> r+ (f1 [r1] k1) (f2 [r2] k2)
     )
     -> a+ (j [a1] f1) (j [a2] f2)
*)
