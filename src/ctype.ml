type ctype =
  | TInt
  | TUnsigned
  | TStruct of int
  | TPtr of ctype
  | TArray of ctype * int
  | TFun of ctype * (ctype list)
