(*reduce*)
fun reduce f z s = List.foldl(fn (x,a) => f a x) z s;

(*squares*)
fun squares (seznam) = List.map (fn x => x*x) seznam;

(*onlyEven*)
fun onlyEven (seznam) = List.filter (fn x => (x mod 2) = 0) seznam;

(*bestString*)
(*fun bestString (string1, string2) = List.foldl (string1, string2);*)
fun bestString f seznam = 
    case seznam of
        [] => ""
    |  g::rep => List.foldl(fn (x,a) => if f (x,a) = true then x else a) g rep;

(*largestString*)
fun largestString (seznam: string list): string =
    bestString(fn (x,y) => x>y) seznam;

(*longestString*)
(*val longestString =
	List.foldl (fn (x, max) => if String.size(x) > String.size(max) then x else max) "";*)
fun longestString (seznam: string list): string =
    bestString(fn (x,y) => String.size(x)>String.size(y)) seznam;

(*quicksort*) (*('a * 'a -> order) -> 'a list -> 'a list*)
fun quicksort (f: 'a * 'a -> order) (seznam: 'a list) = seznam;

(*dot*)
fun dot (xs: int list)(ys: int list)=
    List.foldl (fn (x, y) => x+y) 0 (ListPair.map (fn (x, y) => x*y) (xs, ys));

(*transpose*)
fun transpose (seznam: 'a list list): 'a list list =
    List.tabulate (List.length (List.nth (seznam, 0)),
    fn x => List.map (fn y => (List.nth (y, x))) seznam);

(*multiply*)
fun multiply (a: int list list)(b: int list list) =
    (List.map (fn x => (List.map (fn y => (dot x y)) (transpose b))) a);

(*group*)
(*fun group (xss: ''a list) : (''a * int) list = List.map (fn xs => (hd xs, List.length xs)) xss;*)
fun span_count p x [] count = (count, [])
  | span_count p x (y::xs) count =
    if p (x, y)
    then span_count p x xs (count+1)
    else (count, y::xs);

fun group [] = []
  | group (x::xs) =
    case span_count op= x xs 1 of
      (count, ys) => (x, count) :: group ys;

(*equivalenceClasses*) (*('a -> 'a -> bool) -> 'a list -> 'a list list*)
fun equivalenceClasses (f: 'a -> 'a -> bool) (seznam: 'a list) = [seznam];