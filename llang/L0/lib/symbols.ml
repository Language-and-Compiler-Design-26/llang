type symbol = int

let symbols:(string,symbol) Hashtbl.t = Hashtbl.create 16 

let resolver:(string list) ref = ref [] (* this should be a direct access *)

let get_id sym = List.nth !resolver sym

let get_symbol id = 
  print_endline @@ "Searching for "^id;
  Hashtbl.find symbols id


let last_symbol = ref 0

let next_symbol () = last_symbol:=!last_symbol+1; (!last_symbol-1)

let new_symbol id = 
  match Hashtbl.find_opt symbols id with 
  | None -> 
    print_endline @@ "Added "^id;
    let sym = next_symbol () in
    resolver := (!resolver)@[id];
    Hashtbl.add symbols id sym;
    sym
  | Some sym -> sym 

let string_of_symbol = get_id

(* let string_of_symbol = fun sym -> "_"^string_of_int sym *)