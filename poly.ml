(* Simplification of two plus is sorted version of the two lists concatenated, similar with multiplication *)
(* Can be done in a single simplification but it's more likely to call simplify until it doens't simplify anymore
Multiplication of polynomials -> times list. i.e p(x)^10 = Times([p,p,p,p,p,...]) 
subtraction = Add(T1, Times(Term(-1,0),T2))
*)

(* Times(Times([a;b]);Times([c;d])) = Plus([Times([a;c]);Times(a;d);Times(b;c);Times(b;d)]) *)

(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int (*
      First int is the constant
      Second int is the power of x 
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
      | Plus([]) -> 0
      | Times([]) -> 0
      | Plus(eHd::eTl) ->   (match eTl with
                            | [] -> degree eHd
                            | _-> Stdlib.max (degree eHd) (degree(Plus eTl))
                            )
      | Times(eHd::eTl) ->  (match eTl with
                            | [] -> degree eHd
                            | _-> (degree eHd) + (degree (Times eTl))
                            )

let compare_expr (e1: pExp) (e2: pExp) : int =
  compare (degree e1) (degree e2)
  
let rec sort_pExpList (pList: (pExp list)): (pExp list) =
    List.rev (List.sort compare_expr pList )

(* 
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
let get_coeff (t:pExp):int = match t with 
    | Term(c,d) -> c
    | _ -> failwith "not a term"

let get_deg (t:pExp):int = match t with 
    | Term(c,d) -> d
    | _ -> failwith "not a term"

(* flatten ( Plus([ Term(1,1);
                    Plus([  Plus([  Term(2,2);
                                    Term(5,5)])]);
                    Times([ Term(3,3);
                            Times([ Term(4,4)])]);
                    Plus([Term(6,6);
                          Term(7,7)])]) );;

- : [Term (1, 1); Plus [Term (2, 2); Term (5, 5)]; Times [Term (3, 3); Term (4, 4)]; Term (6, 6); Term (7, 7)]
*)
let rec flatten (e:pExp): (pExp list) = match e with
    | Term(_,_) -> [e]
    | Plus([]) -> []
    | Times([])-> []
    | Plus(outHd::outTl) -> (match outHd with
                            | Term(_,_) -> outHd::(flatten (Plus outTl))
                            | Plus(inList) -> inList@(flatten (Plus outTl))
                            | Times(inList) -> (Times(flatten outHd))::(flatten (Plus outTl))
                            )
    | Times(outHd::outTl) ->(match outHd with
                            | Term(_,_) -> outHd::(flatten (Times outTl))
                            | Plus(inList) -> (flatten outHd)@(flatten (Times outTl))
                            | Times(inList) -> (inList)@(flatten (Times outTl))
                            )

(* (* 
  if both are terms with same degree, return one term with coefs added
  If both are pluses, concat their lists
  if both are times, mult every index of l1 with every index of l2
*)
let add_terms (e1:pExp) (e2:pExp) : pExp = match e1 with
    | Term(c1,d1)  -> if (d1==(get_deg e2)) then Term(((get_coeff e1)+(get_coeff e2)),d1) else Plus([e1;e2])
    | Plus(eList)  -> 
    | Times(eList) ->  *)

let rec mul_terms (pExpList:(pExp list)) : pExp =
  match pExpList with
    | a::b::eTl -> 
    (* after each double match, call mul_terms on newTerm::eTl *)
      (match a with
        | Term(c1, d1) ->
            (match b with
            | Term(c2, d2) -> mul_terms ((Term((c2*c1),(d1+d2)))::eTl) (* Create single term from the two *)
            | Plus(pExprList) -> mul_terms ((Plus(Stdlib.List.map (fun x -> mul_terms [a;x]) pExprList))::eTl) (* Multiply each term in the plus by A term *)
            | Times(pExprList) -> mul_terms ((mul_terms (a::[(mul_terms pExprList)]))::eTl) (* Get term from TIMES, then multiply by A term *)
            )
        | Plus(eList) ->
            (match b with
            | Term(c2, d2) -> mul_terms ((Plus(Stdlib.List.map (fun x -> mul_terms [b;x]) eList))::eTl) (* Multiply each term in the PLUS by A term *)
            | Plus(pExprList) -> mul_terms ((Plus(Stdlib.List.map (fun x-> mul_terms [x;a]) pExprList))::eTl) (* FOIL *)
            | Times(pExprList) -> mul_terms ((mul_terms (a::[(mul_terms pExprList)]))::eTl) (* Get term from TIMES, then distribute into PLUS *)
            )
        | Times(eList) ->
            (match b with
            | Term(c2, d2) -> mul_terms ((mul_terms (b::[(mul_terms eList)]))::eTl) (* Get term from TIMES, then multiply by B term *)
            | Plus(pExprList) -> mul_terms ((mul_terms (b::[(mul_terms eList)]))::eTl) (* Get term from TIMES, then distribute into PLUS *)
            | Times(pExprList) -> mul_terms ((mul_terms (pExprList@eList))::eTl) (* Get term from TIMES, obtain single term from both *)
            )
      )
    | a::[] -> a
    | [] -> failwith "you fucked up"

let rec simplify1 (e:pExp) : pExp = 
  match e with
    | Plus(a::b::eTl) ->
      (match a with
        | Term(c1,d1) ->
          (match b with
            | Term(c2,d2) -> if (d1==d2) then (Plus(Term((c1+c2),d1)::eTl)) else Plus([a;b]@eTl)
            | Plus(pList) -> Plus((sort_pExpList (a::pList))@eTl)
            | Times(pExpList) -> Plus((a::[(mul_terms pExpList)])@eTl)
          )
        | Plus(pExprList) -> simplify1 (Plus(b::pExprList@eTl))
        | Times(a::b::eTl) -> Plus((mul_terms [a;b])::eTl)
      )
    | Times(eList) -> mul_terms eList
    | Plus(a::[]) -> a
    | Times(a::[]) -> a
    | Term(_,_) -> e
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
      (* print_pExp rE; *)
      print_newline();
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)


