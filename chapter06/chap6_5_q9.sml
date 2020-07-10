(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.9
*)

(* 1 *)
fun map f L = foldr (fn (h, R) => f(h)::R) [] L;

fun flatten L = foldr (fn (h, R) => h @ R) [] L;

fun member L x = foldr (fn (h, R) => ((h = x) orelse R)) false L;
(* fun member L x = foldr (fn (h, R) => (h = x)) false []; *)

fun unique L = foldr (fn (h, R) => if member R h then R else h::R) [] L;

(* unique [1,2,3,4,5];
unique [1,2,1,2];
unique [1,1,1,1,1];
unique []; *)

(* いまいちな関数になってしまった *)
(* 
fun prefixList L = 
     case L of nil => [[]]
            | [x] => [[x]]
            | (h::t) => 
                map (fn x => h::x) ([] :: prefixList t);
*)
fun prefixList L = foldr (fn (h, R) => (if R = [[]] then [[h]] else map (fn x => h::x) ([]::R))) [[]] L;

(* fun prefixList L = foldr (fn (h, R) => (map (fn x => h::x) ([]:: R))) [[]] L; *)
prefixList [1,2,3];

(* fun permutations L = 
    let fun insert s nil = [[s]]
         | insert s (h::t) = 
               let val L = insert s t
               in (s::(h::t)) :: (map (fn x => h::x)L)
               end
   in case L of nil => nil
            | [x] => [[x]]
            | (h::t) => 
               let val Pt = permutations t
               in flatten (map (fn x => insert h x) Pt)
               end
   end; *)

fun permutations L = 
    let fun insert s nil = [[s]]
         | insert s (h::t) = 
               let val L = insert s t
               in (s::(h::t)) :: (map (fn x => h::x)L)
               end
   in
        foldr (fn (h, R) => flatten (map (fn x => insert h x) R)) [[]] L
   end;

permutations [1,2,3];