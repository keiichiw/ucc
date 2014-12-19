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
