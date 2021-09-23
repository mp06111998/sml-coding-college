val _ = print("-------- factorial --------");
val _ : int -> int = factorial;
val test1 = factorial 4 = 24;
val test2 = factorial 7 = 5040;
val test3 = factorial 1 = 1;
val test4 = factorial 0 = 0;

val _ = print("-------- power --------");
val _ : int * int -> int = power;
val test1 = power (2, 4) = 16;
val test2 = power (5, 6) = 15625;
val test3 = power (~3, 3) = ~27;
val test4 = power (8, 0) = 1;

val _ = print("-------- gcd --------");
val _ : int * int -> int = gcd;
val test1 = gcd (2, 4) = 2;
val test2 = gcd (12, 14) = 2;
val test3 = gcd (48, 60) = 12;
val test4 = gcd (45, 60) = 15;

val _ = print("-------- len --------");
val _ : int list -> int = len;
val test1 = len [] = 0;
val test2 = len [2, 3, 6, 3, 4, 5, 6, 7, 8, 9, 55, 6, 33] = 13;
val test3 = len [2] = 1;
val test4 = len [11111, 424324] = 2;

val _ = print("-------- last --------");
val _ : int list -> int option = last;
val test1 = last [] = NONE;
val test2 = last [2, 3, 6, 8] = SOME 8;
val test3 = last [2] = SOME 2;
val test4 = last [11111, 424324] = SOME 424324;

val _ = print("-------- nth --------");
val _ : int list * int -> int option = nth;
val test1 = nth ([],5) = NONE;
val test2 = nth ([3, 5, 7], 1) = SOME 5;
val test3 = nth ([~2, 3, ~9, ~6], 3) = SOME ~6;
val test4 = nth ([1, 2, 3, 4], 4) = NONE;

val _ = print("-------- insert --------");
val _ : int list * int * int -> int list = insert;
val test1 = insert ([1, 2, 3], 0, 8) = [8, 1, 2, 3];
val test2 = insert ([], 0, 6) = [6];
val test3 = insert ([~2, 3, ~9, ~6], 3, ~2) = [~2, 3, ~9, ~2, ~6];
val test4 = insert ([1, 2, 3, 4], 4, 2) = [1, 2, 3, 4, 2];

val _ = print("-------- delete --------");
val _ : int list * int -> int list = delete;
val test1 = delete ([1, 2, 3], 1) = [2, 3];
val test2 = delete ([], 0) = [];
val test3 = delete ([~2, 3, ~6, ~9, ~6], ~6) = [~2, 3, ~9];
val test4 = delete ([1, 2, 3, 4], 4) = [1, 2, 3];

val _ = print("-------- reverse --------");
val _ : int list -> int list = reverse;
val test1 = reverse ([1, 2, 3]) = [3, 2, 1];
val test2 = reverse ([]) = [];
val test3 = reverse ([3, ~6, ~9, ~6]) = [~6, ~9, ~6, 3];
val test4 = reverse ([1, 2, 3, 4]) = [4, 3, 2, 1];

val _ = print("-------- palindrome --------");
val _ : int list -> bool = palindrome;
val test1 = palindrome ([1, 2, 3]) = false;
val test2 = palindrome ([]) = true;
val test3 = palindrome ([1, 2, 3, 2, 1]) = true;
val test4 = palindrome ([~5, 6, 6, ~5]) = true;