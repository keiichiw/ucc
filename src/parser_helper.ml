open Ctype

let rec assoc t = function
  | [] -> false
  | (x,_)::_ when x=t -> true
  | _::xs -> assoc t xs

(* typedef *)
let typedef_env:(string * (ctype * bool)) list ref = ref [];;

let is_typedef_name t =
  assoc t !typedef_env

let typedef (Syntax.Decl (_, typ, name, _)) =
  let go = function
    | TUInt -> (TInt, true)
    | TUShort -> (TShort, true)
    | TULong -> (TLong, true)
    | TUChar -> (TChar, true)
    | t -> (t, false) in
  typedef_env := (name, go typ) :: !typedef_env


(* enum *)
let enum_env:(string * int) list ref = ref [];;
let is_enum_id t =
  assoc t !enum_env
let enum_def name v  =
  enum_env := (name, v) :: !enum_env
let get_enum n =
  List.assoc n !enum_env
