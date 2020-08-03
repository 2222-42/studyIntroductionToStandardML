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

(* fun concatDlist dlist1 dlist2 = *)