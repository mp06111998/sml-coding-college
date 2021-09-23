datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun zip ([], _) = []
    | zip (_, []) = []
    | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys);

fun unzip (seznam) = 
    case seznam of 
        [] => ([], [])
        | (a,b)::tl => 
            let val (l1, l2) = unzip tl
            in (a::l1, b::l2) end;

fun subtract (a, b) = 
    case a of
        One => raise NotNaturalNumber
    |   Succ i => (case b of
            One => i
        |   Succ ii => subtract (i, ii)
                );

fun any (f, s) = false;

fun fold (f : 'b * 'a -> 'b, z : 'b, s : 'a list) =
    case s of
        [] => z
    |   h :: t => fold (f, f (z, h), t);

fun foldr (f, z, s) = fold (f, z,
    fold (fn (z, x) => x :: z, [], s));

fun map (f, s) = foldr (fn (z, x) => f x :: z, [], s);

fun filter (f, s) = foldr (fn (z, x) => if f x then x :: z else z, [], s);


fun height lf = 1 | height (br (l,_, r)) = 1 + Int.max (height l, height r);

fun imbalance lf = 0 | imbalance (br (l,_, r)) = height l - height r;

fun rotate (drevo, smer) =
    if (imbalance drevo) = 0 then drevo
    else
        case smer of
            L => (case drevo of
                lf => lf
            |   br (l,s,d) => (case l of
                    lf => drevo
                |   br (ll, ss, dd) => br (br (l, s, ll) , ss, dd)
                )
            )
        |   R => (case drevo of
                lf => lf
            |   br (l,s,d) => (case d of
                    lf => drevo
                |   br (ll, ss, dd) => br (ll, ss, br (dd, s, d))
                )
            );

fun rebalance (drevo) =
    drevo;

fun avl (c, drevo, e) =
    drevo;