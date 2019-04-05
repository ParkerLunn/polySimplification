(* Simplification of two plus is sorted version of the two lists concatenated, similar with multiplication *)
(* Can be done in a single simplification but it's more likely to call simplify until it doens't simplify anymore
Multiplication of polynomials -> times list. i.e p(x)^10 = Times([p,p,p,p,p,...]) 
subtraction = Add(T1, Times(Term(-1,0),T2))
*)


(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int (*
      First int is the constant
      Second int is the power of x 
      10  -> Term(10,0)
      2x -> Term(2,1)
      3x^20 -> Term(3, 20)
    *)
  | Plus of pExp list
  (*
    List of terms added
    Plus([Term(2,1); Term(1,0)])
  *)
  | Times of pExp list (* List of terms multiplied *)

(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
let rec from_expr (_e: Expr.expr) : pExp =
    match _e with
      | Num(n) -> Term(n,0)
      | Var(c) -> Term(1,1)
      | Add(e1,e2) -> Plus([(from_expr e1); (from_expr e2)])
      | Sub(e1,e2) -> Plus([(from_expr e1) ; Times([Term(-1,0); (from_expr e2)])])
      | Mul(e1,e2) -> Times([(from_expr e1); (from_expr e2)])
      | Pow(e,i) -> (match i with
                    | 0-> Term(1,0)
                    | 1-> from_expr e
                    | _ -> Times([(from_expr e); (from_expr (Pow(e,i-1)))])
                    )
      | Pos(e) -> from_expr e;
      | Neg(e) -> Times([Term(-1,0); (from_expr e)])

(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)

let rec degree (_e:pExp): int = match _e with
      | Term(c,d) -> d 
      | Plus(eHd::eTl) ->   (match eTl with
                            | [] -> degree eHd
                            | _-> Stdlib.max (degree eHd) (degree(Plus eTl))
                            )
      | Times(eHd::eTl) ->  (match eTl with
                            | [] -> degree eHd
                            | _-> (degree eHd) + (degree (Times eTl))
                            )

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
let compare (e1: pExp) (e2: pExp) : bool =
  degree e1 > degree e2
  
let rec sort_pExpList (pList: (pExp list)): (pExp list) =
    Sort.list compare pList
(* Print a pExpr nicely 
  Term(3,0) -> 3
  Term(5,1) -> 5x 
  Term(4,2) -> 4x^2
  Plus... -> () + () 
  Times ... -> ()() .. ()

  Hint 1: Print () around elements that are not Term() 
  Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)
let rec print_pExp (_e: pExp): unit = match _e with
    | Term(co,deg) -> (match co with
                      | 0 -> Printf.printf "0"; 
                      | 1 -> Printf.printf ""; (match deg with
                                                | 0 -> Printf.printf "";
                                                | 1 -> Printf.printf "x"; 
                                                | _ -> Printf.printf "x^%d" deg;
                                                )
                      | _ -> Printf.printf "%d" co; (match deg with
                                                    | 0 -> Printf.printf "";
                                                    | 1 -> Printf.printf "x"; 
                                                    | _ -> Printf.printf "x^%d" deg;
                                                    )
                      )
    | Plus(eList) -> (match eList with
                      | eHd::eTl -> (match eTl with
                                    | []-> print_pExp eHd;
                                    | _ -> print_pExp eHd; Printf.printf " + "; print_pExp (Plus eTl );
                                    )
                      | [] -> Printf.printf "\n";
                     )
    | Times(eList)-> (match eList with
                      | eHd::eTl -> (match eTl with
                                    | []-> print_pExp eHd;
                                    | _ -> print_pExp eHd; Printf.printf " * "; print_pExp (Times eTl );
                                    )
                      | [] -> Printf.printf "\n";
                     )
;;

(* 
  Function to simplify (one pass) pExpr

  n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
  Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

  Hint 1: Keep terms in Plus[...] sorted
  Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
  Hint 3: flatten times, i.e. times of times is times
  Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
          Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
  Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
      => Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
      => Plus[Term(2,3); Term(6,5)]
  Hint 6: Find other situations that can arise
*)
let simplify1 (e:pExp): pExp =
    e

(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) :bool = _e1 = _e2

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) in
      print_pExp rE;
      print_newline();
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)


