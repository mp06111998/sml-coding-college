(*val _ = print("-------- reduce --------");
val _ : int list -> int list = reduce;
val test1 = reduce [1,2,3] = [1,4,9];
val test2 = reduce [] = [];
val test3 = reduce [2,2] = [4,4];
val test4 = reduce [8] = [64];
val test5 = reduce [1,8,4,4,1] = [1,64,16,16,1];*)

val _ = print("-------- squares --------");
val _ : int list -> int list = squares;
val test1 = squares [1,2,3] = [1,4,9];
val test2 = squares [] = [];
val test3 = squares [2,2] = [4,4];
val test4 = squares [8] = [64];
val test5 = squares [1,8,4,4,1] = [1,64,16,16,1];

val _ = print("-------- onlyEven --------");
val _ : int list -> int list = onlyEven;
val test1 = onlyEven [1,2,3] = [2];
val test2 = onlyEven [] = [];
val test3 = onlyEven [2,2] = [2,2];
val test4 = onlyEven [8] = [8];
val test5 = onlyEven [1,8,4,4,1] = [8,4,4];

(*bestString*)

(*largestString*)

val _ = print("-------- longestString --------");
val _ : string list -> string = longestString;
val test1 = longestString ["test", "test1", "test11"] = "test11";
val test2 = longestString [] = "";
val test3 = longestString ["t", "tes", "tes"] = "tes";
val test4 = longestString ["test11111", "test1", "test11"] = "test11111";

(*quicksort*)

val _ = print("-------- dot --------");
val _ : int list -> int list -> int = dot;
val test1 = dot [1,2,3] [1,1,1] = 6;
val test2 = dot ([]) ([]) = 0;
val test3 = dot ([2,2]) ([2,2]) = 8;
val test4 = dot ([8]) ([8]) = 64;
val test5 = dot ([1,8]) ([2,2]) = 18;

val _ = print("-------- transpose --------");
val _ : 'a list list -> 'a list list = transpose;
val test1 = transpose [[1,2,3], [2,1,6]] = [[1,2], [2,1], [3,6]];
val test2 = transpose [[1], [1]] = [[1,1]];
val test3 = transpose [[2,2], [2,2]] = [[2,2], [2,2]];
val test4 = transpose [[1,2], [2,1], [3,6]] = [[1,2,3], [2,1,6]];
val test5 = transpose [[1,8], [2,2]] = [[1,2], [8,2]];

(*val _ = print("-------- multiply --------");
val _ : int list list * int list list -> int list list = multiply;
val test1 = multiply ([[3]],[[13]]) = [[39]];
val test2 = multiply ([[3,4,2]],[[13,9,7,15],[8,7,4,6],[6,4,0,3]]) = [[83,63,37,75]];*)

val _ = print("-------- group --------");
val _ : ''a list -> (''a * int) list = group;
val test1 = group [1,1,1,1,1,2] = [(1,5),(2,1)];
val test2 = group [1,3,1,1,1,2] = [(1,1),(3,1),(1,3),(2,1)];
val test3 = group [1,1,1,1,1,1] = [(1,6)];
val test4 = group [] = [];
val test5 = group [3] = [(3,1)];

(*equivalenceClasses*)

