val _ = print "---------- getVars ----------\n";
val _ : ''a expression -> ''a list = getVars;
val test1 = getVars True = [];
val test2 = getVars (Var 1) = [1];
val test3 = getVars (Eq [Var 1, Var 2]) = [1,2];
val test4 = getVars (Or [Var "te", Var "st"]) = ["te", "st"];
val test5 = getVars (And [True, False]) = [];
val test6 = getVars (And [Var "st", Var "st", Not (Var "2")]) = ["st", "2"];
val test7 = getVars (Not (Var "st")) = ["st"];
val test8 = getVars (Imp (Var "te", Var "st")) = ["te", "st"];
val test9 = getVars (Imp (Var "te", Var "te")) = ["te"];

val _ = print "---------- eval ----------\n";
val _ : ''a list -> ''a expression -> bool = eval;
val test1 = eval [1,2] (Var 1) = true;
val test2 = eval [1,2] (True) = true;
val test3 = eval [1,2] (Or [Var 3, True]) = true;
val test4 = eval [1,2] (And [Var 3, True]) = false;
val test5 = eval [1,2] (And [Var 1, Not (Not (Var 2))]) = true;
val test6 = eval [1,2] (And [Var 1, Not(False), Var 2, Var 6]) = false;
val test7 = eval [1,2] (Eq [Var 3, Not(True), False]) = true;
val test8 = eval [1,2] (Eq [Var 3, Not(True), True]) = false;
val test9 = eval [] (Or []) = false;
val test10 = eval [] (And []) = true;
val test11 = eval [] (Eq []) = true;
val test12 = eval [] (Eq[Var 1]) = true;

val _ = print "---------- rmEmpty ----------\n";
val _ : 'a expression -> 'a expression = rmEmpty;
val test1 = rmEmpty (Or []) = False;
val test2 = rmEmpty (And []) = True;
val test3 = rmEmpty (Eq []) = True;
val test4 = rmEmpty (Var "q") = Var "q";
val test5 = rmEmpty (And [Var 3, True]) = (And [Var 3, True]);
val test6 = rmEmpty (False) = False;

val _ = print "---------- beautify ----------\n";
val _ : 'a expression -> 'a expr = beautify;
val test1 = beautify (Var "a") = V "a";
val test2 = beautify (Or [Var 2, Var 4, Var 4]) = V 2 \/ V 4 \/ V 4;
val test3 = beautify (And [Var 2, Var 4]) = V 2 /\ V 4;
val test4 = beautify (True) = T;
val test5 = beautify (Not (Var 3)) = !! (V 3);

val _ = print "---------- pushNegations ----------\n";
val _ : 'a expression -> 'a expression = pushNegations;
val test1 = pushNegations (Not False) = (Not False);
val test2 = pushNegations (Not (Not False)) = False;
val test3 = pushNegations (Not (Or [True, Not False])) = And [Not True, False];

val _ = print "---------- rmConstants ----------\n";
val _ : ''a expression -> ''a expression = rmConstants;
val test1 = rmConstants (Eq [True, Var 1, Var 2]) = (And [Var 1,Var 2]);
val test2 = rmConstants (Eq [Var 1, False, Var 2]) = (And [Not (Var 1),Not (Var 2)]);
val test3 = rmConstants (And [True, Var 1, Var 2]) = (And [Var 1,Var 2]);
val test4 = rmConstants (And [False, Var 1, Var 2]) = False;
val test5 = rmConstants (Or [True, Var 1, Var 2]) = True;
val test6 = rmConstants (Or [False, Var 1, Var 2, False]) = Or [Var 1, Var 2];
val test7 = rmConstants (Not(True)) = False;
val test8 = rmConstants (Not(False)) = True;
val test9 = rmConstants (Eq [Var 1, Var 2]) = (Eq [Var 1,Var 2]);
val test10 = rmConstants (Imp(Var 1, False)) = Not (Var 1);
val test11 = rmConstants (Imp(Var 1, True)) = True;
val test12 = rmConstants (Imp(False, Var 1)) = True;
val test13 = rmConstants (Imp(True, Var 1)) = Var 1;
val test14 = rmConstants (Imp(True, Var 1)) = Var 1;
val test100 = rmConstants (Eq [Var 1, Eq [False, Var 3, And [And [], Or [Var 1, Not (Eq [Var 4, False, True])], 
Imp (True, Var 2)]]]) = (Eq [Var 1,And [Not (Var 3),Not (Var 2)]]);

val _ = print "---------- rmVars ----------\n";
val _ : ''a expression -> ''a expression = rmVars;
val test1 = rmVars (Var "a") = Var "a";
val test2 = rmVars (Or [Var 1, Var 2, Var 1]) = (Or [Var 1, Var 2]);
val test3 = rmVars (And [Var 1, Var 2, Var 1]) = (And [Var 1, Var 2]);
val test4 = rmVars (Eq [Var 1, Var 2, Var 1]) = (Eq [Var 1, Var 2]);
val test5 = rmVars (Eq [Not (Var 3), Var 1, Not (Var 3)]) = (Eq [Not (Var 3), Var 1]);

(*simplify*)

(*DODATNO*)

val _ = print "---------- toWolframLang ----------\n";
val _ : ('a -> string) -> 'a expression -> string = toWolframLang;
val test1 = toWolframLang Int.toString (Var 3) = "Var[\"3\"]";
val test2 = toWolframLang Int.toString (Not(Var 3)) = "Not[Var[\"3\"]]";
val test3 = toWolframLang Int.toString (Imp (True, Var 2)) = "Implies[True, Var[\"2\"]]";
val test4 = toWolframLang Int.toString (Eq [True, Eq [False, Var 3, And [And [], Or [Var 1, Not (Eq [Var 4, False, True])], Imp (True, Var 2)]]])
= "Equivalent[True, Equivalent[False, Var[\"3\"], And[And[], Or[Var[\"1\"], Not[Equivalent[Var[\"4\"], False, True]]], Implies[True, Var[\"2\"]]]]]";
val test5 = toWolframLang (fn x => x)
    (Eq [Eq [False, Var "a", And [And [], Or [Var "b", Not (Eq [Var "test"])],
        Imp (True, Var "2")]]]) =
      "Equivalent[Equivalent[False, Var[\"a\"], And[And[], Or[Var[\"b\"], Not[Equivalent[Var[\"test\"]]]], Implies[True, Var[\"2\"]]]]]";