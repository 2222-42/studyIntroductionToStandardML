(* SML source file. Copyright (c) by 2222-42 2020.
* Q10.11
*)

signature FUNQUEUE = sig
    exception EmptyQueue
    type 'a queue
    val newQueue: unit -> 'a queue
    val enqueue: 'a * 'a queue -> 'a queue
    val dequeue: 'a queue -> 'a * 'a queue
end;
(* 筆者はテキストと異なり、exceptionを導入しているので、こちらでも導入。 *)

(* 第一の方針: 直接定義する *)
(* 1-1: 不透明なシグネチャ制約 *)
(* structure IntQueue :> FUN_QUEUE where type 'a = int = struct *)
(* stdIn:181.45-181.52 Error: syntax error: deleting  EQUALOP IDA EQUALOP *)
(* structure IntQueue :> FUN_QUEUE where type 'a queue = int list = struct 
...
end
*)
(* Error: type queue does not match definitional specification *)
(* structure IntQueue :> FUNQUEUE where type 'a queue = 'a list = struct
    exception EmptyQueue
    type 'a queue = 'a list 
    fun newQueue() = nil : 'a queue
    fun enqueue (item, q) = (item :: q) : 'a queue
    fun removeLast nil = raise EmptyQueue
      | removeLast [x] = (nil, x)
      | removeLast (h::t) =
        let val (t',last) = removeLast t
        in (h::t',last)
        end
    fun dequeue q =
        let val (rest, last) = removeLast q
        in (last, rest : 'a queue)
        end
end; *)
(* 
stdIn:1.2-161.4 Error: value type in structure does not match signature spec
    name: enqueue
  spec:   'a * 'a ?.IntQueue.queue -> 'a ?.IntQueue.queue
  actual: 'a * 'a list -> 'a list
stdIn:1.2-161.4 Error: value type in structure does not match signature spec
    name: dequeue
  spec:   'a ?.IntQueue.queue -> 'a * 'a ?.IntQueue.queue
  actual: 'a list -> 'a * 'a list 

'a からの型変換がおかしいので、この方針はやめる
-> 問題文が、「汎用の待ち行列ストラクチャを作成せよ」だったので、もしかして'aと型変数にしたままでよい？
*)

structure IntQueue :> FUNQUEUE = struct
    exception EmptyQueue
    type 'a queue = 'a list 
(* = type int queue = int list
stdIn:193.10-193.21 Error: syntax error: deleting  IDA IDA EQUALOP *)
    fun newQueue() = nil : 'a queue
    fun enqueue (item, q) = (item :: q) : 'a queue
    fun removeLast nil = raise EmptyQueue
      | removeLast [x] = (nil, x)
      | removeLast (h::t) =
        let val (t',last) = removeLast t
        in (h::t',last)
        end
    fun dequeue q =
        let val (rest, last) = removeLast q
        in (last, rest : 'a queue)
        end
end;

(* 筆者の解答: *)
   structure Queue :> FUNQUEUE =
   struct
     exception EmptyQueue
     type 'a queue = 'a list
     fun newQueue() = nil : 'a queue
     fun enqueue (item,queue) = item :: queue
     fun dequeue nil = raise EmptyQueue
       | dequeue [x] = (x, nil)
       | dequeue (h::t) =
         let val (last, t') =  dequeue t
         in (last, h::t')
         end
   end


(* val q = IntQueue.newQueue();
(* structure IntQueue :> FUN_QUEUE where type 'a queue = 'a list = struct
...
end;
stdIn:1.6-1.29 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...) *)
 *)
val q = IntQueue.newQueue() : int IntQueue.queue;
(* これでええんか？
-> 筆者の解答を見ると、これでいいらしい。
ただし、IntQueueが必ずしもint型に限定されていない、型変数のままなので、どうにかしないといけない。
*)
val q = IntQueue.enqueue(1,q)
val q = IntQueue.enqueue(2,q)
val q = IntQueue.enqueue(3,q)
val (pop, last) = IntQueue.dequeue q;
val (pop, last) = IntQueue.dequeue last;

structure FastIntQueue :> FUNQUEUE = struct
    exception EmptyQueue
    type 'a queue = 'a list * 'a list
    fun newQueue() = ([], []) : 'a queue
    fun enqueue (i, (a,b)) = (i::a, b) : 'a queue
    fun dequeue ([], []) = raise EmptyQueue
      | dequeue (list, []) = 
          let val (h::t) = rev list
          in (h, ([], t))
          end
      | dequeue (list,(h::t)) = (h, (list, t))
end;

(* 筆者の解答: *)
   structure FastQueue :> FUNQUEUE = struct
     exception EmptyQueue
     type 'a queue = 'a list * 'a list
     fun newQueue () = ([],[]) : 'a queue
     fun enqueue (i,(a,b))  = (i :: a, b)
     fun dequeue ([],[]) = raise EmptyQueue
       | dequeue (L, []) =
           let
               val (h::t) = rev L
           in
              (h, ([], t))
           end
      | dequeue (a,h::t) = (h, (a,t))
   end

val q = FastIntQueue.newQueue() : int FastIntQueue.queue;
val q = FastIntQueue.enqueue(1,q)
val q = FastIntQueue.enqueue(2,q)
val q = FastIntQueue.enqueue(3,q)
val (pop, last) = FastIntQueue.dequeue q;
val (pop, last) = FastIntQueue.dequeue last;

(* 第二の方針: ファンクターを定義する *)
(* functor FunQueueFUN(type 'a queue) :> FUN_QUEUE where type 'a queue = 'a list = struct *)
(* functor FunQueueFUN(type 'a) :> FUN_QUEUE where type 'a = int = struct
    exception EmptyQueue
    type 'a queue = 'a list
    fun newQueue() = nil : 'a queue
    fun enqueue (item, q) = (item :: q) : 'a queue
    fun removeLast nil = raise EmptyQueue
      | removeLast [x] = (nil, x)
      | removeLast (h::t) =
        let val (t',last) = removeLast t
        in (h::t',last)
        end
    fun dequeue q =
        let val (rest, last) = removeLast q
        in (last, rest : 'a queue)
        end
end;

structure IntQueue = FunQueueFUN(type 'a queue = int list); *)
(* val q = IntQueue.newQueue();
(* stdIn:122.5-122.28 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
val q = [] : ?.X1 IntQueue.queue *)
map (fn x => IntQueue.enqueue(x,q)) [1,3,5];
IntQueue.dequeue q; *)
(* 
これもqueueとlistの型情報の不一致が発生する。
-> `FunQueueFUN(type 'a queue) :> FUN_QUEUE where type 'a queue = 'a list`として解決？
*)
(* 
functor FunQueueFUN(type 'a queue) :> FUN_QUEUE where type 'a queue = 'a queue = struct
    exception EmptyQueue
    type 'a queue = 'a list
    fun newQueue() = ([]) : 'a queue
    fun enqueue (i, (a,b)) = (i::a, b)
    fun dequeue ([], []) = raise EmptyQueue
      | dequeue (list, []) = 
          let val (h::t) = rev list
          in (h, ([], t))
          end
      | dequeue (list,(h::t)) = (h, (list, t))
end; *)
