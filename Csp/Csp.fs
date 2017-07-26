module Csp

open System

type Variables         = string list 

(*Scope is a tuple of variables that participate in the constraint*)
[StructuralEqualityAttribute,StructuralComparisonAttribute]
type Scope              = string * string
(*every list in Domain coresponds to a Variable *)
and Domains<'d>         = list<'d>
(* a relation can be expressed explicitely *)
(* for instance ((X_1,X_2),[(A,B),(B,A)])*)
(*where X_1 and X_2 are Variables and {A,B} Domains*)
and ConstraintExpl<'a> = (string * string) * ('a * 'a) list
(*or implicetly: ((X_1,X_2),X_1 != X_2)*)
and ConstraintImpl<'a> = Scope * ('a -> 'a -> bool) 


(* a costraint satisfaction problem can be modelled as triple CSP(X,D,C) where *)
(*X is a set of variables, D a set of domains for each variable and
C a set of constraints*)
//type CspI  = { x : Variables ; d: Domain<int> list; c: ConstraintExpl<int> list}
type CspExpl = { x0 : Variables ; d0: Domains<int> list; c0: ConstraintExpl<int> list }
(* a more general version of CSP where 'a is the domain type and 'b  *)
type Csp<'a> = { x : Variables ; d: Domains<'a> list; c: ConstraintImpl<'a> list}
type Arc = string * string

module CspExamples = 
  let ex01Expl = {x0 = ["A";"B"]
                ; d0 = [[1;2];[1;2]]
                ; c0 = [("A","B"),[(1,2);(2,1)]] }
  
  let ex01Impl = {x = ["A";"B"]; d = [[1;2];[1;2]]; c = [("A","B") , fun a b -> a <> b] }
  // constraint Y = X^2
  let ex02Expl = {x0 = ["X";"Y"]
                 ;d0 = [[0;1;2;3];[0;1;4;9]]
                 ;c0 = [(("X","Y"),[(0,0);(1,1);(2,4);(3,9)])]}
  let ex02Impl = {x = ["X";"Y"]
                 ;d = [[0;1;2;3];[0;1;4;9]]
                 ;c = [("X","Y"), fun x y -> y = x*x ] }

  //type MapColoring = { V : Variables ; D: Domain<string> list; C: ConstraintImpl<int> list }
  
 
  let notEq = ( <> )
  let mapColCsp: Csp<string> = 
    { x =["WA";"NT";"Q";"NSW";"V";"SA";"T"]
     ;d = List.replicate 7 ["red";"green";"blue"]
     ;c = [("SA","WA"), notEq;("SA","NT"), notEq 
     ;("SA","Q"), notEq; ("SA","NSW"), notEq
     ;("SA","V"), notEq; ("WA","NT"), notEq
     ;("NT","Q"), notEq; ("Q","NSW"), notEq
     ;("NSW","V"), notEq ]}  
let getArcs (csp: Csp<'a>) : Arc list = List.map(fun (tup, fn) -> tup) csp.c

///Arc consistency,reduce the domain of variables mantaining arc consistency
///of the whole CSP

//Mackworth's AC-3 algorithm
let revise(csp: Csp<'a>, xi:string, xj:string): bool =
  true
let AC3 (csp: Csp<'a>) : bool = 
  //lookup table (key,value) = (var,domain)
  let domains = List.zip csp.x csp.d |> dict 
  let queue : Arc list = getArcs csp//queue of arcs that contains all the arcs in csp
  
  let rec queueNonEmpty (q: Arc List) doms (result:bool): bool =
    match q with
    |(xi,xj)::tl -> 
      if revise(csp,xi,xj) then 
        if ((domains.Item xi) |> List.isEmpty) then false else 
          let newq = List.pick(fun (i,j) -> 
            match (i,j) with
            | (i,j) when i = xi -> Some (i,j)
            | _ -> None ) tl
          (queueNonEmpty newq doms result)
      else true
    | [] -> result
    
  queueNonEmpty queue domains false

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
