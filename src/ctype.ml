type ctype =
  | TInt
  | TShort
  | TLong
  | TUnsigned
  | TChar
  | TVoid
  | TStruct of int
  | TPtr of ctype
  | TArray of ctype * int
  | TFun of ctype * (ctype list)

type linkage =
  | Static
  | Extern
  | NoLink

let is_funty = function
  | TFun _ -> true
  | _ -> false
