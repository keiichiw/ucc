open Ctype

let rec assoc t = function
  | [] -> false
  | (x,_)::_ when x=t -> true
  | _::xs -> assoc t xs

(* typedef *)
let typedef_env:(string * ctype) list ref = ref [];;

let is_typedef_name t =
  assoc t !typedef_env
let typedef (Syntax.Decl (_, typ, Name name, _)) =
  typedef_env := (name, typ) :: !typedef_env


(* enum *)
let enum_env:(string * int) list ref = ref [];;
let is_enum_id t =
  assoc t !enum_env
let enum_def name v  =
  enum_env := (name, v) :: !enum_env
let get_enum n =
  List.assoc n !enum_env
