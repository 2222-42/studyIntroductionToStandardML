(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 10.1
*)

structure IntQueue = struct
    exception EmptyQueue
    type queue = int list ref
    (* 空の待ち行列を作成 *)
    fun newQueue() = ref nil : queue
    (* 末尾に要素を追加 *)
    fun enqueue (item, queue) = queue := item :: (!queue)
    fun removeLast nil = raise EmptyQueue
      | removeLast [x] = (nil, x)
      | removeLast (h::t) =
        let val (t',last) = removeLast t
        in (h::t',last)
        end
    (* 待ち行列の先頭の要素を取り除き、その要素を返す。 *)
    fun dequeue queue =
        let val (rest, last) = removeLast (!queue)
        in (queue:=rest; last)
        end
end
(* 
structure IntQueue :
  sig
    exception EmptyQueue
    type queue = int list ref
    val newQueue : unit -> queue
    val enqueue : 'a * 'a list ref -> unit
    val removeLast : 'a list -> 'a list * 'a
    val dequeue : 'a list ref -> 'a
  end
*)

(* ストラクチャ名の明示的指定によって、モジュール内の資源を利用できる *)
val q = IntQueue.newQueue();
map (fn x => IntQueue.enqueue(x,q)) [1,3,5];
IntQueue.dequeue q;

(* open文で、そのストラクチャ内の全ての名前の束縛が現在の環境に追加され、直接利用可能になる。 *)
open IntQueue;
dequeue q;
