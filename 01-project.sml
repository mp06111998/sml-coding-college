datatype 'a expr = !! of 'a expr
                | \/ of 'a expr * 'a expr
                | /\ of 'a expr * 'a expr
                | <=> of 'a expr * 'a expr
                | ==> of 'a expr * 'a expr
                | V of 'a
                | T | F;
infix 5 <=>;
infixr 6 ==>;
infix 7 \/;
infix 8 /\;

datatype 'a expression = Not of 'a expression
                    | Or of 'a expression list
                    | And of 'a expression list
                    | Eq of 'a expression list
                    | Imp of 'a expression * 'a expression
                    | Var of 'a
                    | True | False;

datatype 'a stream = Next of 'a * (unit -> 'a stream);
fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed) end;

fun int2bool i = LargeInt.mod (i, 2) = 1;

exception InvalidCNF;
exception NotImplemented;



fun getVars (izraz: ''a expression) : ''a list=
    let fun pomozna (izraz: ''a expression, acc: ''a list) : ''a list=
        case izraz of
            Var q => if List.exists (fn (x) => x = q) acc then acc else acc@[q]
        |   True => acc
        |   False => acc
        |   Or k => List.foldl (fn (x, acc) => pomozna(x,acc)) acc k
        |   And l => List.foldl (fn (x, acc) => pomozna(x,acc)) acc l
        |   Eq m => List.foldl (fn (x, acc) => pomozna(x,acc)) acc m
        |   Not n => pomozna(n,acc)
        |   Imp terka => List.foldl (fn (x, acc) => 
        (if x = 1 then pomozna((#1 terka),acc) else pomozna((#2 terka),acc))
        ) acc [1,2]
    in pomozna (izraz, [])
    end;

fun eval (spremenljivke:''a list) (izraz:''a expression) : bool=
    case izraz of
        Var q => if List.exists (fn (x) => x = q) spremenljivke then true else false
    |   True => true
    |   False => false
    |   Not n => if (eval spremenljivke n = true) then false else true
    |   Or k => List.foldr (fn (x, y) => eval spremenljivke x orelse y) false k
    |   And l => List.foldr (fn (x, y) => eval spremenljivke x andalso y) true l
    |   Eq m => if List.length m = 0 then true else if List.length m = 
        (
            #1 (List.foldl (fn (x, (acc, h)) => if eval spremenljivke x = eval spremenljivke h then (acc+1,h) else (acc,h)) (0, (hd m)) m)
        )
        then true else false
    |   Imp terka => eval spremenljivke (Or [Not(#1 terka), (#2 terka)]);

fun rmEmpty (izraz: 'a expression) : 'a expression=
    case izraz of
        Var q => Var q
    |   True => True
    |   False => False
    |   Not n => Not (rmEmpty n)
    |   Or k => 
    if (length k) = 0 then False else if (length k) = 1 then (rmEmpty (hd k)) else
    Or (List.map (fn (x) => rmEmpty x) k)
    |   And l => 
    if (length l) = 0 then True else if (length l) = 1 then (rmEmpty (hd l)) else
    And (List.map (fn (x) => rmEmpty x) l)
    |   Eq m => 
    if (length m) = 0 then True else if (length m) = 1 then True else
    Eq (List.map (fn (x) => rmEmpty x) m)
    |   Imp terka => Imp (rmEmpty (#1 terka), rmEmpty (#2 terka));
    
fun beautify (izraz: 'a expression) : 'a expr = 
    case rmEmpty izraz of
        Var q => V q
    |   True => T
    |   False => F
    |   Or (g::rep) => List.foldl (fn (x, acc) => acc \/ (beautify x)) (beautify g) rep
    |   And (g::rep) => List.foldl (fn (x, acc) => acc /\ (beautify x)) (beautify g) rep
    |   Eq (g::rep) => #1 (List.foldl (fn (x, (acc, prej)) => case acc of
                                                        T => ((prej <=> (beautify x)), (beautify x))
                                                    |   _ => (acc /\ (prej <=> (beautify x)), (beautify x)) ) (T, (beautify g)) rep)
    |   Not n => !! (beautify n)
    |   Imp terka => (beautify (#1 terka)) ==> (beautify (#2 terka));

fun pushNegations(izraz: 'a expression) : 'a expression = 
    case rmEmpty izraz of
        Not q => (case q of
                    Var j => Not (Var j)
                |   True => Not (True)
                |   False => Not (False)
                (*Naslednih 6 je tistih De Morganovi zakoni z ucilnice*)
                |   Not a => pushNegations(a)
                |   And b => Or (List.map (fn (x) => pushNegations(Not x)) b)
                |   Or c => And (List.map (fn (x) => pushNegations(Not x)) c)
                |   Imp terka => And [pushNegations(#1 terka), pushNegations(Not (#2 terka))]
                |   Eq d => And [Or (List.map (fn (x) => pushNegations(Not x)) d), Or (List.map (fn (x) => pushNegations(x)) d)])
    |   Var j => Var j
    |   True => True
    |   False => False
    |   Or k => Or (List.map (fn (x) => pushNegations(x)) k)
    |   And l => And (List.map (fn (x) => pushNegations(x)) l)
    |   Eq m => Eq (List.map (fn (x) => pushNegations(x)) m)
    |   Imp terka => Imp (pushNegations (#1 terka), pushNegations (#2 terka));

fun findT(lis: ''a expression list): bool = if (List.foldl (fn (x, acc) => if x = True then acc+1
else acc) 0 lis) > 0 then true else false;
fun findF(lis: ''a expression list): bool = if (List.foldl (fn (x, acc) => if x = False then acc+1
else acc) 0 lis) > 0 then true else false;
fun rmConstants(izraz: ''a expression) : ''a expression =
    case rmEmpty izraz of
        Var q => Var q
    |   True => True
    |   False => False
    |   Not n => if rmConstants n = False then True else if rmConstants n = True then False else (Not (rmConstants n))
    |   Or k => if (findT k)
     then True else rmConstants(Or (List.foldl (fn (x, acc) => if x = False then acc else acc@[rmConstants x]) [] k))
    |   And l => if (findT l) 
    then rmConstants(And (List.foldl (fn (x, acc) => if x = True then acc else acc@[rmConstants x]) [] l)) else False
    |   Eq m => if (findT(m) andalso findF(m)) then False else if findT(m) then 
    (And (List.foldl (fn (x, acc) => if x = True then acc else acc@[(rmConstants x)]) [] m))
    else if findF(m) then (And (List.foldl (fn (x, acc) => if x = False then acc else acc@[(Not (rmConstants(x)))]) [] m))
    else (Eq (List.map (fn (x) => rmConstants(x)) m))
    |   Imp terka => case (rmConstants(#1 terka)) of
                    True => (rmConstants(#2 terka))
                |   False => True
                |   _ => case (rmConstants(#2 terka)) of
                        True => True
                    |   False => (Not(rmConstants (#1 terka)))
                    |   _ => Imp ((rmConstants(#1 terka)), (rmConstants(#2 terka)));

fun rmVars (izraz: ''a expression) : ''a expression=
    case rmEmpty izraz of
        Var q => Var q
    |   True => True
    |   False => False
    |   Not n => Not (rmVars n)
    |   Or k => rmEmpty (Or (List.foldl (fn (x, acc) => if (List.exists (fn (xx) => (rmVars xx) = (rmVars x)) acc) then acc else acc@[rmVars x]) [] k))
    |   And l => rmEmpty (And (List.foldl (fn (x, acc) => if (List.exists (fn (xx) => (rmVars xx) = (rmVars x)) acc) then acc else acc@[rmVars x]) [] l))
    |   Eq m => rmEmpty (Eq (List.foldl (fn (x, acc) => if (List.exists (fn (xx) => (rmVars xx) = (rmVars x)) acc) then acc else acc@[rmVars x]) [] m))
    |   Imp terka => if (rmVars (#1 terka)) = (rmVars (#2 terka)) then True else Imp (rmVars (#1 terka), rmVars (#2 terka));


fun simplify(izraz: ''a expression) : ''a expression = 
    let val shrani = rmVars(pushNegations(rmConstants izraz))
    in if izraz = shrani then izraz
    else (simplify shrani)
    end;
    
fun prTestEq _ _ _ = raise NotImplemented;


fun printList xs = if xs <> nil then (#1(List.foldl (fn (x, (acc, ali)) => if ali then (acc ^ x, false) else (acc ^ ", " ^ x, false)) ("", true) xs)) else "";
fun toWolframLang (funk:('a -> string)) (izraz:'a expression): string =
    case izraz of
        Var q => "Var[\"" ^ (funk q) ^ "\"]"
    |   True => "True"
    |   False => "False"
    |   Not n => "Not[" ^ (toWolframLang (funk) (n)) ^ "]"
    |   Or k => "Or[" ^ (printList (List.map (fn (x) => (toWolframLang (funk) (x))) k)) ^ "]"
    |   And l => "And[" ^ (printList (List.map (fn (x) => (toWolframLang (funk) (x))) l)) ^ "]"
    |   Eq m => "Equivalent[" ^ (printList (List.map (fn (x) => (toWolframLang (funk) (x))) m)) ^ "]"
    |   Imp terka => "Implies[" ^ (toWolframLang (funk) (#1 terka)) ^ ", " ^ (toWolframLang (funk) (#2 terka)) ^ "]";


fun satSolver _ = raise NotImplemented;

fun bruteforce _ = raise NotImplemented;


type timetable = {day : string, time: int, course: string} list;
type student = {studentID : int, curriculum : string list};


fun problemReduction _ _ _ = raise NotImplemented;

fun solutionRepresentation _ = raise NotImplemented;