(* v datoteki program-tests.sml *)

val _ = print("-------- next --------");
val _ : int -> int = next;
val test1 = next 51 = 52;
val test2 = next ~18 = ~17;
val test3 = next ~1 = 0;

val _ = print("-------- add --------");
val _ : int * int -> int = add;
val test1 = add (6, 8) = 14;
val test2 = add (~5, ~7) = ~12;
val test3 = add (~3, 5) = 2;

val _ = print("-------- majority --------");
val _ : bool * bool * bool -> bool = majority;
val test1 = majority (true, true, false) = true;
val test2 = majority (true, false, true) = true;
val test3 = majority (false, true, true) = true;
val test4 = majority (false, true, false) = false;
val test4 = majority (false, false, false) = false;

val _ = print("-------- median --------");
val _ : real * real * real -> real = median;
val test1 = Real.== (median (6.0, 8.0, 12.0), 8.0);
val test2 = Real.== (median (~3.0, ~8.0, 12.0), ~3.0);
val test3 = Real.== (median (6.0, 12.0, 6.0), 6.0);

val _ = print("-------- triangle --------");
val _ : int * int * int -> bool = triangle;
val test1 = triangle (8, 7, 20) = false;
val test2 = triangle (3, 6, 4) = true;
val test3 = triangle (4, 22, 7) = false;