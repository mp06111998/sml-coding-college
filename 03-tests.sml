val _ = print("-------- zip --------");
val _ : 'a list * 'b list -> ('a * 'b) list = zip;
val test1 = zip ([1,2,3],["a","b","c"]) = [(1,"a"),(2,"b"),(3,"c")];
val test2 = zip ([],[1]) = [];
val test3 = zip (["a"],[]) = [];
val test4 = zip (["b"],[1,2]) = [("b",1)];

val _ = print("-------- unzip --------");
val _ : ('a * 'b) list -> 'a list * 'b list = unzip;
val test1 = unzip ([(1,"a"),(2,"b"),(3,"c")]) = ([1,2,3],["a","b","c"]);
val test2 = unzip ([]) = ([],[]);
val test3 = unzip ([("b",1)]) = (["b"],[1]);

val _ = print("-------- subtract --------");
val _ : natural * natural -> natural = subtract;
(*val test1 = subtract (One, One) = NotNaturalNumber;
val test2 = subtract (One, Succ (One)) = NotNaturalNumber;*)
val test3 = subtract (Succ (One), One) = One;
val test4 = subtract (Succ (Succ (One)), One) = Succ (One);
val test5 = subtract (Succ (Succ (One)), Succ (One)) = One;