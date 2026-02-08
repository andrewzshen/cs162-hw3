open Ast

let todo () = failwith "TODO"

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
    (* This line imports the functions in Vars, so you can write [diff .. ..]
       instead of [Vars.diff .. ..] *)
    let open Vars in
    (* Your code goes here *)
    match e with
    | Num _ -> empty
    | Binop (_, e1, e2) -> union (free_vars e1) (free_vars e2)
    | Var x -> singleton x
    | Lambda binder ->
        let x, body = binder in
        diff (free_vars body) (singleton x)
    | App (e1, e2) -> union (free_vars e1) (free_vars e2)
    | Let (e1, binder) ->
        union (free_vars e1)
            (let x, body = binder in
            diff (free_vars body) (singleton x))
    (* booleans *)
    | True -> empty
    | False -> empty 
    | IfThenElse (e1, e2, e3) -> union (union (free_vars e1) (free_vars e2)) (free_vars e3) 
    | Comp (_, e1, e2) -> union (free_vars e1) (free_vars e2)  
    (* lists *)
    | ListNil -> empty 
    | ListCons (e1, e2) -> union (free_vars e1) (free_vars e2) 
    | ListMatch (e1, e2, (x, (y, e3))) ->
        let s1 = union (free_vars e1) (free_vars e2) in
        let s2 = diff (diff (free_vars e3) (singleton x)) (singleton y) in
        union s1 s2 
    (* recursion *)
    | Fix (x, e) -> diff (free_vars e) (singleton x) 

(** Perform substitution c[x -> e], i.e., substituting x with e in c *)
let rec subst (x : string) (e : expr) (c : expr) : expr =
    match c with
    | Num n -> Num n
    | Binop (op, c1, c2) -> Binop (op, subst x e c1, subst x e c2)
    | Var y -> if x = y then e else Var y
    | Lambda binder ->
        let y, body = binder in
        Lambda (y, if String.equal x y then body else subst x e body )
    | App (c1, c2) -> App (subst x e c1, subst x e c2)
    | Let (c1, binder) ->
        Let (subst x e c1,
            let y, body = binder in
            let body' = if String.equal x y then body else subst x e body in 
            (y, body'))
    (* booleans *)
    | True -> empty
    | False -> empty 
    | IfThenElse (e1, e2, e3) -> union (union (free_vars e1) (free_vars e2)) (free_vars e3) 
    | Comp (_, e1, e2) -> union (free_vars e1) (free_vars e2)  
    (* lists *)
    | ListNil -> empty 
    | ListCons (e1, e2) -> union (free_vars e1) (free_vars e2) 
    | ListMatch (e1, e2, (x, (y, e3))) ->
        let s1 = union (free_vars e1) (free_vars e2) in
        let s2 = diff (diff (free_vars e3) (singleton x)) (singleton y) in
        union s1 s2 
    (* recursion *)
    | Fix (x, e) -> diff (free_vars e) (singleton x) 

(** Evaluate expression e *)
let rec eval (e : expr) : expr =
  try match e with _ -> todo ()
  with Stuck msg ->
    im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.expr e)
