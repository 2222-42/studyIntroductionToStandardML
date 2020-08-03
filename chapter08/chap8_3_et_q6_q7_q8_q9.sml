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
(* fun concatDlist dlist1 dlist2 = *)