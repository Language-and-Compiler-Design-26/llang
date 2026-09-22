open Symbols 

type 'a env = (symbol * 'a) list list

let empty = []

let begin_scope env = [] :: env

(* 
  [end_scope env] is the environment after ending the current scope.
  pre: [env <> []] 
*)
let end_scope = function
  | [] -> assert false (* Precondition failed: No scope to end *)
  | _::rest -> rest

let add_binding id v = function
  | [] -> assert false (* Precondition failed: No scope to add binding to *)
  | scope::rest -> ((id, v)::scope) :: rest

let rec lookup id = function
  | [] -> failwith ("Unbound identifier: " ^ (string_of_symbol id))
  | scope::rest ->
      match List.assoc_opt id scope with
      | Some v -> v
      | None -> lookup id rest

let rec lookup_opt id = function
  | [] -> None
  | scope::rest ->
      match List.assoc_opt id scope with
      | Some v -> Some v
      | None -> lookup_opt id rest