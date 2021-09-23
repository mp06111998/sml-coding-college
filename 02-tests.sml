val _ = print("-------- neg --------");
val _ : number -> number = neg;
val test1 = neg Zero = Zero;
val test2 = neg (Pred Zero) = (Succ Zero);
val test3 = neg (Succ Zero) = (Pred Zero);
val test4 = neg (Succ (Succ Zero)) = (Pred (Pred Zero));

val _ = print("-------- add --------");
val _ : number * number -> number = add;
val test1 = add (Zero, Zero) = Zero;
val test2 = add ((Pred Zero), (Pred Zero)) = (Pred (Pred Zero));
val test3 = add ((Succ (Succ Zero)), (Pred Zero)) = (Succ (Succ (Pred Zero)));
val test4 = add ((Zero), (Pred Zero)) = (Pred Zero);

val _ = print("-------- comp --------");
val _ : number * number -> order = comp;
val test1 = comp (Zero, Zero) = EQUAL;
val test2 = comp ((Pred Zero), Zero) = LESS;
val test3 = comp ((Succ Zero), Zero) = GREATER;
val test4 = comp ((Succ (Pred Zero)), Zero) = EQUAL;

val _ = print("-------- contains --------");
val _ : tree * int -> bool = contains;
val test1 = contains (Leaf 8, 8) = true;
val test2 = contains (Leaf 7, 8) = false;
val test3 = contains ((Node (5, Leaf 2, Leaf 4)), 2) = true;
val test4 = contains ((Node (5, (Node (5, Leaf 6, Leaf 4)), Leaf 4)), 3) = false;

val _ = print("-------- countLeaves --------");
val _ : tree -> int = countLeaves;
val test1 = countLeaves (Leaf 8) = 1;
val test2 = countLeaves (Leaf 7) = 1;
val test3 = countLeaves (Node (5, Leaf 2, Leaf 4)) = 2;
val test4 = countLeaves (Node (5, (Node (5, Leaf 6, Leaf 4)), Leaf 4)) = 3;

val _ = print("-------- countBranches --------");
val _ : tree -> int = countBranches;
val test1 = countBranches (Leaf 8) = 0;
val test2 = countBranches (Leaf 7) = 0;
val test3 = countBranches (Node (5, Leaf 2, Leaf 4)) = 2;
val test4 = countBranches (Node (5, (Node (5, Leaf 6, Leaf 4)), Leaf 4)) = 4;

val _ = print("-------- height --------");
val _ : tree -> int = height;
val test1 = height (Leaf 8) = 1;
val test2 = height (Leaf 7) = 1;
val test3 = height (Node (5, Leaf 2, Leaf 4)) = 2;
val test4 = height (Node (5, (Node (5, (Node (5, Leaf 6, Leaf 4)), Leaf 4)), Leaf 4)) = 4;

val _ = print("-------- toList --------");
val _ : tree -> int list = toList;
val test1 = toList (Leaf 8) = [8];
val test2 = toList (Leaf 7) = [7];
val test3 = toList (Node (5, Leaf 2, Leaf 4)) = [2,5,4];
val test4 = toList (Node (7, (Node (5, Leaf 6, Leaf 4)), Leaf 8)) = [6,5,4,7,8];;