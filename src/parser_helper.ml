let typedef_env:(string * Syntax.ctype) list ref = ref [];;
let is_typedef_name t =
  let rec go = function
    | [] -> false
    | (x,_)::_ when x=t -> true
    | _::xs -> go xs in
  go !typedef_env
let typedef (Syntax.DVar (typ, Syntax.Name name, _)) =
  typedef_env := (name, typ) :: !typedef_env
