module Csp


open System

type Variables         = string list 

(*Scope is a tuple of variables that participate in the constraint*)
and Scope              = string * string
(*every list in Domain coresponds to a Variable *)
and Domain<'d>         = list<'d>
(* a relation can be expressed explicitely *)
(* for instance ((X_1,X_2),[(A,B),(B,A)])*)
(*where X_1 and X_2 are Variables and {A,B} Domains*)
and ConstraintExpl<'a> = (string * string) * ('a * 'a) list
(*or implicetly: ((X_1,X_2),X_1 != X_2)*)
and ConstraintImpl<'a> = Scope * ('a -> 'a -> bool) 


(* a costraint satisfaction problem can be modelled as triple CSP(X,D,C) where *)
(*X is a set of variables, D a set of domains for each variable and
C a set of constraints*)
type Csp  = { x : Variables ; d: Domain<int> list; c: ConstraintExpl<int> list}
type Csp2 = { x2 : Variables ; d2: Domain<int> list; c2: ConstraintImpl<int> list }

type Arc = string * string

module CspExamples = 
  let ex01Expl = {x = ["A";"B"]
                ; d = [[1;2];[1;2]]
                ; c = [("A","B"),[(1,2);(2,1)]] }
  
  let ex01Impl = {x2 = ["A";"B"]; d2 = [[1;2];[1;2]]; c2 = [("A","B") , fun a b -> a <> b] }
  // constraint Y = X^2
  let ex02Expl = {x = ["X";"Y"]
                 ;d = [[0;1;2;3];[0;1;4;9]]
                 ;c = [(("X","Y"),[(0,0);(1,1);(2,4);(3,9)])]}
  let ex02Impl = {x2 = ["X";"Y"]
                 ;d2 = [[0;1;2;3];[0;1;4;9]]
                 ;c2 = [("X","Y"), fun x y -> y = x*x ] }
  type MapColoring = { V : Variables ; D: Domain<string> list; C: ConstraintImpl<int> list }
  let notEq = fun a b -> (a <> b) 
  let mapColCsp = { V =["WA";"NT";"Q";"NSW";"V";"SA";"T"]
                    ;D = List.replicate 7 ["red";"green";"blue"]
                    ;C = [("SA","WA"), notEq;("SA","NT"), notEq 
                    ;("SA","Q"), notEq; ("SA","NSW"), notEq
                    ;("SA","V"), notEq; ("WA","NT"), notEq
                    ;("NT","Q"), notEq; ("Q","NSW"), notEq
                    ;("NSW","V"), notEq ]}
let getArcs (csp: Csp) : Arc list = 
  let {x = X; d = D; c = C} = csp
  [("A","B");("C","D")]

///Arc consistency,reduce the domain of variables mantaining arc consistency
///of the whole CSP

let AC3 (csp: Csp) : bool = 
  let {x = xs; d = ds; c = cs} = csp
  let queue : Arc list = []
  //queue of arcs that contains all the arcs in csp
  true

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code