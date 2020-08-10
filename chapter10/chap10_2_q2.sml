(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 10.1 Q10.2 Q10.3 Q10.4
*)

signature QUEUE = sig
    exception EmptyQueue
    type queue
    val newQueue: unit -> queue
    val enqueue: (int*queue) -> unit
    val dequeue: queue -> int
end

(* signatureはspec扱いにできて、ここに定義されていない関数は隠蔽される。 *)
structure IntQueue : QUEUE = struct
    exception EmptyQueue
    type queue = int list ref
    fun newQueue() = ref nil : queue
    fun enqueue (item, queue) = queue := item :: (!queue)
    fun removeLast nil = raise EmptyQueue
      | removeLast [x] = (nil, x)
      | removeLast (h::t) =
        let val (t',last) = removeLast t
        in (h::t',last)
        end
    fun dequeue queue =
        let val (rest, last) = removeLast (!queue)
        in (queue:=rest; last)
        end
end

val q = IntQueue.newQueue();
map (fn x => IntQueue.enqueue(x,q)) [1,3,5];
IntQueue.dequeue q;
(* IntQueue.removeLast q;
Error: unbound variable or constructor: removeLast in path IntQueue.removeLast *)

IntQueue.enqueue (2,q);
q := [1];
IntQueue.dequeue;
(* 型情報を不透明にしたい。そのシグネチャで宣言された関数を通じてのみ利用できるようにしたい *)

structure AbsIntQueue :> QUEUE = IntQueue;
val q = AbsIntQueue.newQueue();
AbsIntQueue.enqueue(1,q);
AbsIntQueue.dequeue q;
(* q := [1]; *)


(* Q10.5 *)
structure AbsFastIntQueue :> QUEUE = struct
    exception EmptyQueue
    (* newItemsとoldItemsに分割する。
    待ち行列全体はnewItemsとoldItemsの逆順リストとを連結したものに等しくなるようにする。 *)
    type queue = int list ref * int list ref
    (* 空の待ち行列を作成 *)
    fun newQueue() = (ref [], ref []) : queue
    (* 末尾に要素を追加。newItemsリストの先頭に要素を付け加える *)
    fun enqueue (i, (a,b)) = a := i :: (!a)
    (* 待ち行列の先頭の要素を取り除き、その要素を返す。 *)
    (* 
      - oldList が空でなければ
        - その先頭を取り除き
        - 取り除いた値を返す
      - oldItemsが空でかつnewItemsが空でなければ、
        - newItemsの逆順のリストを作成し、
          - それの先頭要素を取り除き、
          - 取り除いたリストを新たにoldItemsにし、
        - newItemsを空にし
        - 取り除いた値を返す
      - 両方からだったら、EmptyQueue例外を発生させる。
    *)
    fun dequeue (ref[], ref[]) = raise EmptyQueue
      | dequeue (a as ref L, b as ref []) = 
          let val (h::t) = rev L
          in (a:=nil; b:=t; h)
          end
      | dequeue (a,b as ref (h::t)) = (b := t; h)
end

val r = AbsFastIntQueue.newQueue();
AbsFastIntQueue.enqueue(1,r);
AbsFastIntQueue.dequeue r;
(* r := [1]; *)

local
  (* structure Q = AbsIntQueue *)
  structure Q = AbsFastIntQueue
in
  val test2 = Q.newQueue();
  val result = (map (fn x => Q.enqueue(x,test2)) [1,3,5]; Q.dequeue test2);
end;

(* 

datatype文はは新しい型とその型のデータ構成子を同時に定義する。

structureでtype文で型の宣言をする。
-> 透明なシグネチャ制約にする。
-> データ構成子は隠蔽されてしまう。

型構成子の使用をユーザーに許したい場合は、
-> シグネチャでdatatype文全体を含めておきましょう。
*)
