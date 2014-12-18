type ctype =
  | TInt
  | TUnsigned
  | TChar
  | TStruct of int
  | TPtr of ctype
  | TArray of ctype * int
  | TFun of ctype * (ctype list)
