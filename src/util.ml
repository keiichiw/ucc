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
