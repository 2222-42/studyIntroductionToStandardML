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

(* 筆者の解答 *)
fun prefixListByAuthor L =
    foldr (fn (x,l)=> [x]::(map (fn y => x::y) l)) nil L;
(*
回答者のコメント:
- 元のprefixListに関する理解や実装によって、ここまで単純さに違いが出る良い例
- 自分の解答だと、"Warning: calling polyEqual"が出てしまうが、筆者の解答例だとそれがでない。
*)

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

(* 筆者の解答 *)
fun permutationsByAuthor L =
    let fun insertAll s nil = [[s]]
          | insertAll s (h::t) =
              let val L = insertAll s t
              in (s::(h::t)) :: (map (fn x => h::x) L)
              end
    in foldr (fn (x,y) => foldr (fn (a,b) => insertAll x a @ b) nil y)
             [nil]
             L
    end;

(* 2 *)

fun exists L P = foldr (fn (h, R) => (P h) orelse R) false L;

fun forall L P = foldr (fn (h, R) => (P h) andalso R) true L;


(* 3 *)
val sumList = foldr (fn (h, R) => h + R) 0;
(* fun prefixList L = foldr (fn (h, R) => (if R = [[]] then [[h]] else map (fn x => h::x) ([]::R))) [[]] L; *)
fun prefixSum L = foldr (fn (h, R) => map (fn x => h + x) (0::R)) [] L;
prefixSum [1,2,3,4,5];

fun prefixSumByAuthor L =
    foldr (fn (x,y) => x::(map (fn z => z + x) y)) nil L;
(* 
回答者のコメント:
- 元のprefixListに関する理解や実装によって、ここまで単純さに違いが出る良い例2
*)
