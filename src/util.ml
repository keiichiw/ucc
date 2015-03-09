let sum_of = List.fold_left (+) 0
let max_of = List.fold_left max 0

let (<<) f g = fun x -> f (g x)
let (>>) f g = fun x -> g (f x)

let push stack x = stack := x :: !stack
let peek stack = List.hd !stack
let pop stack = stack := List.tl !stack

let id x = x
let const x _ = x

let rec rep x = function
  | 0 -> []
  | n -> x :: rep x (n-1)

let rec take = function
  | 0 -> const []
  | i -> function
    | []   -> failwith "take"
    | x::xs -> x::take (i-1) xs

let rec list_set = function
  | [] -> const (const [])
  | x :: xs -> function
    | 0 ->
       fun y -> y :: xs
    | i ->
       fun y -> x :: list_set xs (i - 1) y

let is_none x =
  x = None

let is_some x =
  not (is_none x)

let from_some = function
  | Some x -> x
  | None -> failwith "from_some"

let opMap f = function
  | Some x -> Some (f x)
  | None -> None

let opMap2 f a b =
  match (a, b) with
  | (Some x, Some y) ->
     Some (f x y)
  | _ -> None

module String = struct
  include String
  let slice str s e =
    let s = if s < 0 then length str + s else s in
    let s = max 0 (min (length str) s) in
    let e = if e < 0 then length str + e else e in
    let e = max s (min (length str) e) in
    sub str s (e - s)
end
