type pExp =
  | Term of int*int (*
      First int is the constant
      second is poewr of x
      10 -> Term(10,0)
      2x^20 -> Term(2,20)
      *)
  | Plus of pExp list
    (*
    List of terms added
    Plus([Term(2,1);Term(1,0)]) 
    *)
  | Times of pExp list 
    (*
      List of terms multiplied
    *)