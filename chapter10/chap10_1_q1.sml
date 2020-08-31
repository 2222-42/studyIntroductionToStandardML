(* SML source file. Copyright (c) by 2222-42 2020.
* Q 10.1
*)

structure ImperativeIntQueue = struct
    exception EmptyQueue
    datatype 'a cell = NIL | CELL of {data: 'a, left: 'a cell ref, right: 'a cell ref}
    type 'a dlist = 'a cell ref
    (* 空の待ち行列を作成 *)
    fun newQueue() = ref NIL : int dlist
    (* 末尾に要素を追加 *)
    fun enqueue (a, dlist) =
        case dlist of
            ref (CELL{left=l1 as ref (CELL{right=r1,...}),...}) =>
                let val cell = CELL{data=a, right=ref(!dlist), left=ref(!l1)}
                in (dlist:=cell; l1:=cell; r1:=cell)
                end
          | ref NIL =>
                let 
                    val l = ref NIL
                    val r = ref NIL
                    val cell = CELL{data=a, left=l, right=r}
                in 
                    (dlist:=cell; l:=cell; r:=cell)
                end
    fun dataDlist (ref (CELL{data=d, ...})) = d
      | dataDlist (ref NIL) = raise EmptyQueue
    fun rightDlist (ref (CELL{right,...})) = right
      | rightDlist (ref NIL) = raise EmptyQueue
    fun leftDlist (ref (CELL{left,...})) = left
      | leftDlist (ref NIL) = raise EmptyQueue
    fun deleteDlist dlist =
        case dlist of
          ref NIL => raise EmptyQueue
        | ref (CELL{left=l1 as ref (CELL{right=r2,left=l2,...}),
                    right=r1 as ref (CELL{right=r3,left=l3,...}),
                    ...}) => 
              if l1 = l2 then dlist := NIL
              else (dlist := !r1; r2 := !r1; l3 := !l1);
    (* 待ち行列の先頭の要素を取り除き、その要素を返す。 *)
    fun dequeue queue =
        let 
            val last = leftDlist queue
            val data = dataDlist last
        in (deleteDlist last; data)
        end
end

val q = ImperativeIntQueue.newQueue();
map (fn x => ImperativeIntQueue.enqueue(x,q)) [1,3,5];
q;
ImperativeIntQueue.dequeue q;
(* 
- ImperativeIntQueue.dequeue q;
val it = 1 : int
- ImperativeIntQueue.dequeue q;
val it = 1 : int
- ImperativeIntQueue.dequeue q;
val it = 1 : int
- ImperativeIntQueue.dequeue q;
=> ずらしていなかったのが原因

uncaught exception Match [nonexhaustive match failure]
  raised at: chapter10\chap10_1_q1.sml:28.48
=> leftDlistなどのところでなされていなかった。
uncaught exception EmptyQueue
  raised at: chapter10\chap10_1_q1.sml:29.37-29.47
*)
