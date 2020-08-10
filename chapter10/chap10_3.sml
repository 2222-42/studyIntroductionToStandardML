(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 10.1 Q10.2 Q10.3 Q10.4
*)

(* 一部の型情報だけ公開したい *)
signature POLY_QUEUE = sig
    exception EmptyQueue
    type elem
    type queue
    val newQueue: unit -> queue
    val enqueue: (elem*queue) -> unit
    val dequeue: queue -> elem
end
(* 
signature POLY_QUEUE =
  sig
    exception EmptyQueue
    type elem
    type queue
    val newQueue : unit -> queue
    val enqueue : elem * queue -> unit
    val dequeue : queue -> elem
  end
*)

structure IntQueue :> POLY_QUEUE = struct
    exception EmptyQueue
    type elem = int
    type queue = elem list ref
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
(* 
structure IntQueue :> POLY_QUEUE = struct
...
end;

- val q = IntQueue.newQueue();
val q = - : IntQueue.queue
- map (fn x => IntQueue.enqueue(x,q)) [1,3,5];
stdIn:2.1-2.44 Error: operator and operand do not agree [overload conflict]
  operator domain: IntQueue.elem list
  operand:         [int ty] list
  in expression:
    (map (fn x => IntQueue.enqueue <exp>)) (1 :: 3 :: 5 :: nil)
*)

structure StringQueue :> POLY_QUEUE = struct
    exception EmptyQueue
    type elem = string
    type queue = elem list ref
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

structure CQueue :> POLY_QUEUE where type elem = char = struct
    exception EmptyQueue
    type elem = char
    type queue = elem list ref
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
end;
(* structure CQueue : POLY_QUEUE? *)
open CQueue;
(* opening CQueue
  exception EmptyQueue
  type elem = char
  type queue
  val newQueue : unit -> queue
  val enqueue : elem * queue -> unit
  val dequeue : queue -> elem 

ここでのelem型はchar型 の別名である。
*)

val q = newQueue();
(* val q = - : IntQueue.queue *)
map (fn x => enqueue(x,q)) [#"a",#"b",#"c"];
dequeue q;
