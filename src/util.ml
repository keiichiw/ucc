let sum_of = List.fold_left (+) 0
let max_of = List.fold_left max 0

let (<<) f g = fun x -> f (g x)
let (>>) f g = fun x -> g (f x)

let push stack x = stack := x :: !stack
let peek stack = List.hd !stack
let pop stack = stack := List.tl !stack

let id x = x

let rec rep x = function
  | 0 -> []
  | n -> x :: rep x (n-1)

let opMap f = function
  | Some x -> Some (f x)
  | None -> None

let opMap2 f a b =
  match (a, b) with
  | (Some x, Some y) ->
     Some (f x y)
  | _ -> None
