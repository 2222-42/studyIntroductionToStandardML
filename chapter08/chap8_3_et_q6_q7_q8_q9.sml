(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 8.3　Q8.6
*)

(* 参照型とデータ型定義を組み合わせて循環2重リンクリストを作るよ *)
datatype 'a cell = NIL | CELL of {data: 'a, left: 'a cell ref, right: 'a cell ref}

type 'a dlist = 'a cell ref;


(* Q8.6 *)
fun dataDlist (ref (CELL{data=d, ...})) = d
(* stdIn:1.6-118.26 Warning: match nonexhaustive
          ref (CELL {data=d,left=_,right=_}) => ... 
val dataDlist = fn : 'a cell ref -> 'a
*)

fun rightDlist (ref (CELL{right,...})) = right;
    (* case dlist of 
        ref (CELL(data=d1, right=r1 as ref(CELL{data=d2, right=r2, left=l2})), left=l1)=>
            let
                val cell = CELL{data=d2, right=ref(!r2), left=ref(CELL{data=d1, left=l1, })}
            in
                (dlist:=cell; r1:=r2; l1:=)
            end 
    これだと無限に定義が伸びる        
    *)

fun leftDlist (ref (CELL{left,...})) = left;

fun insertDlist a dlist =
    case dlist of
        ref (CELL{left=l1 as ref (CELL{right=r1,...}),...}) =>
            let val cell = CELL{data=a, right=ref(!dlist), left=ref(!l1)}
            in (dlist:=cell; l1:=cell; r1:=cell)
            end
      | ref NIL =>
            let val l = ref NIL
                val r = ref NIL
                val cell = CELL{data=a, left=l, right=r}
            in (dlist:=cell; l:=cell; r:=cell)
            end;
(*             in (dlist:=cell; ())
ではダメな理由
*)

fun singletonDlist a = 
    let val l = ref NIL
        val r = ref NIL
        val cell = CELL{data=a, left=l, right=r}
    in (r:=cell; l:=cell; r)
    end;

Control.Print.printDepth := 20;
val test = singletonDlist 0;
insertDlist 1 test;
insertDlist 2 test;
dataDlist test; 
(* expected: 2 *)
dataDlist (rightDlist test); 
(* expected: 1 *)
dataDlist (leftDlist test); 
(* expected: 0 *)

(* Q8.7 *)
fun deleteDlist dlist =
    case dlist of
        ref (CELL {right=r as ref (CELL{left=l1,...}), left=l as ref (CELL {right=r2,...}),...}) 
            => if !l = !dlist then dlist := NIL
               else (dlist := !r; r2 := !r; l1 := !l)
      | ref NIL => ();
(* 
以下は消す方針での実装
        ref (CELL {right=r1 as ref (CELL{data=a,right=r2,left=l2}),...}) =>
            let
                val cell = CELL{data=a, right=ref(!r2), left=ref(!dlist)}
            in
                (dlist:=cell; l2:= cell; r1:= cell)
            end
上記だとうまく消せない
-> 右と左をつなげる、でやろう

fun deleteDlist dlist =
    case dlist of
        ref (CELL {right=r as ref (CELL{right=r1,left=l1,...}), left=l as ref (CELL {right=r2,left=l2,...}),...}) 
            => (dlist :=!r; r := !r1; l := !l2)
      | ref NIL => ();
以下の怒られ
  rule domain: 'Z cell ref
  object: _ -> 'Z cell ref -> 'Y
  in expression:
    (case dlist
      of ref
           (CELL
             {left=l as ref (CELL {left=l2,right=r2,...}),
              right=r as ref (CELL {left=l1,right=r1,...}),...}) =>
           ((dlist <errorvar>) r; r := ! r1; l := ! l2)
       | ref NIL => ())
逆になってる

fun deleteDlist dlist =
    case dlist of
        ref (CELL {right=r as ref (CELL{left=l1,...}), left=l as ref (CELL {right=r2,...}),...}) 
            => (dlist := !r; r2 := !r; l1 := !l)
      | ref NIL => ();
これだと、空にならない。

val test = singletonDlist 0;
insertDlist 1 test;
val r1 = rightDlist test;
test = r1;
deleteDlist test;
val r1 = rightDlist test;
test = r1;
deleteDlist test;

val test = singletonDlist 0;
insertDlist 0 test;
val r1 = rightDlist test;
val l1 = leftDlist test;
test = r1;
test = l1;
r1 = l1;
!test = !r1;
!test = !l1;
!r1 = !l1;
*)

test;
deleteDlist test;
test;
deleteDlist test;
test;
deleteDlist test;
test;


(* fromListToDlist : 'a list -> 'a dlist*)
fun fromListToDlist list = foldr (fn (h, R) => (insertDlist h R; R)) (ref NIL) list;
(* 
- fun fromListToDlist list = foldl (fn (h, R) => insertDlist h R) (ref NIL) list;
stdIn:18.28-18.79 Error: operator and operand do not agree [tycon mismatch]
  operator domain: 'Z * 'Z cell ref -> 'Z cell ref
  operand:         'Z * 'Z cell ref -> unit
  in expression:
    foldl (fn (h,R) => (insertDlist h) R)

insertDlistの返り値はunit。

singletonDlist の実装を参考にしてみよう。
*)


val listTest = fromListToDlist [1,2,3];
dataDlist listTest; 
(* expected: 1 *)
dataDlist (rightDlist listTest); 
(* expected: 2 *)
dataDlist (leftDlist listTest); 
(* expected: 3 *)
(* expected: true *)

(* Q8.8 *)
fun concatDlist dlist1 dlist2 =
    case (dlist1, dlist2) of
            (ref NIL, _) => (dlist1 := !dlist2)
          | (_, ref NIL) => (dlist2 := !dlist1)
          | (d1 as ref (CELL {right=r1 as ref (CELL{right=r11,...}), left=l1 as ref (CELL {right=r12,...}),...}),
            d2 as ref (CELL {right=r2 as ref (CELL{right=r21,...}), left=l2 as ref (CELL {right=r22,...}),...}) )
                => let 
                    val previousL1 = !l1
                    val previousR11 = !r11
                   in
                    (l1 := !l2; l2 := previousL1; r11 := !r21; r21 := previousR11)
                   end;

(* (l1 := !l2; l2 := !l1; )
としたいけれど、問8.1で見たように参照型で、評価順序によって値が変わるので、保存しておく必要がある。
-> 以下のようにしてみた
    => let 
        val previousL1 = !l1
        val previousR1 = !r1
        in
        (l1 := !l2; l2 := previousL1; r1 := !r2; r2 := previousR1; d1)
        end;
これだと思ったような形式にならない。
-> 中身の参照を変えないといけないから、以下のようにした。
        (l1 := !l2; l2 := previousL1; r11 := !r21; r21 := previousR11; d1)

以下のように定義すると、まず型が意図しているものと違うという問題が生じる。
            (ref NIL, _) => dlist2
          | (_, ref NIL) => dlist1
          | ...
                    (l1 := !l2; l2 := previousL1; r11 := !r21; r21 := previousR11; d1)
                   end;
val concatDlist = fn : 'a cell ref -> 'a cell ref -> 'a cell ref

次に、両方の連結リストを更新するということも、片方がref NILの場合に満たせていない。
*)
val test = (ref NIL: int dlist);
val test0 = singletonDlist 0;
concatDlist test0 test;
val test11 = singletonDlist 1;
concatDlist test0 test11;
dataDlist test0; 
(* expected: 1 *)
dataDlist (rightDlist test0); 
(* expected: 0 *)
dataDlist (rightDlist(rightDlist test0));
(* expected: 1 *)
dataDlist test; 
(* expected: 0 *)
dataDlist (rightDlist test); 
(* expected: 1 *)
dataDlist (rightDlist(rightDlist test));
(* expected: 0 *)


val test2 = singletonDlist 2;
val test3 = singletonDlist 3;
concatDlist test2 test3;
dataDlist test2; 
(* expected: 3 *)
dataDlist (rightDlist test2); 
(* expected: 2 *)

concatDlist test0 test2;
dataDlist test0; 
(* expected: 3 *)
dataDlist (rightDlist test0); 
(* expected: 2 *)
dataDlist (rightDlist (rightDlist test0)); 
(* expected: 1 *)
dataDlist (leftDlist test0); 
(* expected: 0 *)
dataDlist (leftDlist (leftDlist test0)); 
(* expected: 1 *)

(* 
確認したこと
fun concatDlist dlist1 dlist2 =
    case (dlist1, dlist2) of
            (ref NIL, _) => dlist2
          | (_, ref NIL) => dlist1
          | (d1 as ref (CELL {right=r1 as ref (CELL{right=r11,...}), left=l1 as ref (CELL {right=r12,...}),...}),
            d2 as ref (CELL {right=r2 as ref (CELL{right=r21,...}), left=l2 as ref (CELL {right=r22,...}),...}) )
                => let 
                    val previousL1 = !l1
                    val previousR1 = !r1
                   in
                    (l1 := !l2; l2 := previousL1; r1 := !r2; r2 := previousR1; d1)
                   end;
- dataDlist concatResult; 
val it = 1 : int
- dataDlist (rightDlist concatResult); 
val it = 2 : int
- dataDlist (leftDlist concatResult); 
val it = 2 : int

期待しているものと違う

                    (l1 := !l2; r1 := !r2; l2 := previousL1; r2 := previousR1; d1)
評価順序を変えても変わらない
*)

(* 筆者の回答 *)
   fun concatDlist D1 D2 =
       case (D1, D2) of
         (ref NIL, _) => (D1 := !D2; D1)
        | (_, ref NIL) => (D2 := !D1; D1)
        | (ref (CELL{left=d1l as ref (CELL{right=d1lr,...}),...}),
           ref (CELL{left=d2l as ref (CELL{right=d2lr,...}),...})) =>
           let
             val d1lCell = !d1l
             val d1lrCell = !d1lr
           in
             (d1l := !d2l;
              d1lr := !d2lr;
              d2l := d1lCell;
              d2lr := d1lrCell;
              D1)
           end;
(* 回答者のコメント:
回答者の回答はref NILの場合にいずれの循環リストも更新する実装になっていなかった。
筆者の回答は、問で検討している型と異なる。回答者の型も問題文で意図しているものと違う。
    改善点: 型を正しくしよう。
回答者の回答はrightの中のrightを置き換えているが、必ずしもそれは必要ではなく、leftの中のrightを置き換えるだけで十分である。
*)

val test = (ref NIL: int dlist);
val test0 = singletonDlist 0;
concatDlist test0 test;

(* 改善案 *)
   fun concatDlist D1 D2 =
       case (D1, D2) of
         (ref NIL, _) => (D1 := !D2)
        | (_, ref NIL) => (D2 := !D1)
        | (ref (CELL{left=d1l as ref (CELL{right=d1lr,...}),...}),
           ref (CELL{left=d2l as ref (CELL{right=d2lr,...}),...})) =>
           let
             val d1lCell = !d1l
             val d1lrCell = !d1lr
           in
             (d1l := !d2l;
              d1lr := !d2lr;
              d2l := d1lCell;
              d2lr := d1lrCell)
           end;


(* 循環構造だから、空になるまで処理を繰り替えすということはできない。 *)

fun member x list = case list of nil => false
                            | (h::t) => if h = x then true else member x t;

fun dlistToList L = 
    let fun f l visited = 
        if member l visited then nil 
        else (dataDlist l)::(f (rightDlist l) (l::visited))
    in f (rightDlist (leftDlist L)) nil 
    end;

(* 問8.9 *)
fun copyDlist DL = 
    let val list = dlistToList DL
    in fromListToDlist list
    end;

(* val it = fn : ('a -> 'b) -> 'a ref cell -> 'b ref cell *)
(* fun mapDlist g L =
    let 
        fun f l visited = 
            if member l visited then l
            else (case l of
                    (ref NIL) => l
                  | (ref (CELL{data=d, left, right})) => 
                        let
                            val newL = ref NIL
                            val visited = l::visited
                            val le = f left visited
                            val ri = f right visited
                        in
                            (newL := CELL{left = le, right = ri, data = g d};
                            newL)
                        end
                )
    in 
        f (rightDlist (leftDlist L)) nil 
    end; *)
(* GIVE UP *)
(* 筆者の回答：
これら関数の適切な定義には、以下の点を含む種々の注意深い考察が必要である。

- 循環構造を辿る際の終了条件の適切な判定
- leftおよびrightフィールドをたどると自分自身に戻ってくる CELLへの参照型データの作成
*)

fun mapDlist f d =
    let
        (* 回答者のコメント:
        newElemを使って、終了条件を判定している。
        そして、ただ真偽を判定するのではなく、結果に渡すべきDlistも出している。*)
        fun newElem x nil = NONE
        | newElem x ((h,newH)::t) =
            if x = h then SOME newH
            else newElem x t
        fun copy l copied =
            case l of
            ref NIL => ref NIL
            | ref (CELL{left, right, data}) =>
            (case newElem l copied of
                NONE =>
                let
                    val newL = ref NIL
                    (* 空のnewLを入れておいて、inの中で更新している *)
                    val copied = (l, newL)::copied
                    val l = copy left copied
                    val r = copy right copied
                in
                    (newL := CELL{left = l, right = r, data = f data};
                    newL)
                end
            | SOME newL => newL
            )
    in
        (* 以下のように、最初の要素の重複を避けるためにリンク内のポインタからスタートするようにしていないのはなぜだろうか？
            copy (rightDlist (leftDlist d)) nil
        *)
        copy d nil
    end;
(* なんでこれが思いつくのかが全く分からない。
筋は通っている。
*)

val test = fromListToDlist [1,2,3];
dataDlist test; 
dataDlist (rightDlist test); 
dataDlist (rightDlist (rightDlist test)); 
dataDlist (rightDlist (rightDlist (rightDlist test))); 

val result = mapDlist (fn x => x + 1) test; 
dataDlist result; 
dataDlist (rightDlist result); 
dataDlist (rightDlist (rightDlist result)); 
dataDlist (rightDlist (rightDlist (rightDlist result))); 
dataDlist (rightDlist (rightDlist (rightDlist (rightDlist result)))); 

(* 
try1:
fun mapDlist g L =
    let fun f l visited = 
        if member l visited then nil 
        else (concatDlist 
        (singletonDlist (g (dataDlist l))) 
        (deleteDlist l;l);
        (f (rightDlist l) (l::visited)))
    in f (rightDlist (leftDlist L)) nil 
    end;
これだと止まらなくなっちゃう。
*)
(* 
try2:
fun mapDlist g L =
    let fun f l visited = 
        if member l visited then ()
        else (case l of
                (ref NIL) => ()
              | (ref (CELL{data=d,...})) => (d := g(!d))
            );(f (rightDlist l) (l::visited))
    in f (rightDlist (leftDlist L)) nil 
    end;
val it = fn : ('a -> 'a) -> 'a ref cell ref -> unit
となってしまう。

これだと、他のポインタに反映されていない。
*)
(* 
try3:
fun mapDlist g L =
    let 
        fun f l visited = 
            if member l visited then l
            else (case l of
                    (ref NIL) => l
                  | (ref (CELL{data=d, left, right})) => 
                        let
                            val newL = ref NIL
                            val visited = l::visited
                            val le = f left visited
                            val ri = f right visited
                        in
                            (newL := CELL{left = le, right = ri, data = g d};
                            newL)
                        end
                )
    in 
        f (rightDlist (leftDlist L)) nil 
    end;
*)

(* 筆者の回答: mapDlistを使ったケース *)
fun copyDlistByAuthor d = mapDlist (fn x => x) d;
