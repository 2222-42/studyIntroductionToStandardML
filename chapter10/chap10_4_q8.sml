(* SML source file. Copyright (c) by 2222-42 2020.
* Chap Q10.8
*)
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

fun fromPreOrder s =
  let fun decompose s = 
      let 
        fun searchLP s p = 
          if substring(s, p, 1) = "(" then p
          else searchLP s (p+1)
        fun searchRP s p n = 
          case (substring(s, p, 1), n) 
          of (")", 0) => p
          | (")", n) => searchRP s (p+1) (n-1)
          | ("(", n) => searchRP s (p+1) (n + 1)
          | (_, n) => searchRP s (p + 1) n
        val lp1 = searchLP s 0
        val rp1 = searchRP s (lp1 + 1) 0
        val lp2 = searchLP s (rp1 + 1)
        val rp2 = searchRP s (lp2 + 1) 0
      in
        (
          substring (s, 0, lp1),
          substring (s, lp1 + 1, rp1-lp1-1),
          substring (s, lp2 + 1, rp2-lp2-1)
        )
      end
  in if s = "" then Empty
     else let val (root, left, right) = decompose s
          in Node(root, fromPreOrder left, fromPreOrder right)
          end
  end;

signature POLY_QUEUE = sig
    exception EmptyQueue
    type elem
    type queue
    val newQueue: unit -> queue
    val enqueue: (elem*queue) -> unit
    val dequeue: queue -> elem
end;

structure STQueue :> POLY_QUEUE where type elem = string tree = struct
    exception EmptyQueue
    type elem = string tree
    type queue = elem list ref * elem list ref
    fun newQueue() = (ref [], ref []) : queue
    fun enqueue (i, (a,b)) = a := i :: (!a)
    fun dequeue (ref[], ref[]) = raise EmptyQueue
      | dequeue (a as ref L, b as ref []) = 
          let val (h::t) = rev L
          in (a:=nil; b:=t; h)
          end
      | dequeue (a,b as ref (h::t)) = (b := t; h)
end;

structure BF = struct
    structure Q = STQueue
    fun bf t = 
        let val queue = Q.newQueue()
            fun loop() = 
                (case Q.dequeue queue of
                    Node(data, l, r) => (Q.enqueue (l, queue); Q.enqueue(r, queue); data::loop())
                  | Empty => loop()
                ) handle Q.EmptyQueue => nil
        in (Q.enqueue (t, queue); loop() )
        end
end;

(* 関数型待ち行列によって、string treeを要素とする待ち行列を作れ *)
(* 関数型の待ち行列を作って、それによってstring treeを要素とする待ち行列を作れ？ *)
(* 「関数型」という単語で意図するところがわからなかったので、とりあえず参照型をなくした。 *)
(* cf: https://vfoley.xyz/functional-queues/ *)
(* cf: https://www.cs.cornell.edu/courses/cs3110/2012sp/recitations/rec07.html *)
(* 参照型がないと、値の更新ができないからダメじゃね？ *)

(* signature FUNCTIONAL_POLY_QUEUE = sig
    exception EmptyQueue
    type elem
    type queue
    val isEmpty: queue -> bool
    val newQueue: unit -> queue
    val enqueue: (elem*queue) -> queue
    val dequeue: queue -> (elem*queue)
end;

structure FunctionalQueue :> FUNCTIONAL_POLY_QUEUE where type elem = string tree = struct
    exception EmptyQueue
    type elem = string tree
    type queue = elem list * elem list 
    fun isEmpty([], []) = true
      | isEmpty(_, _) = false
    fun newQueue() = ([], []) : queue
    fun enqueue ((i:elem), ((a,b):queue)) = (i::a,b)
    fun dequeue ([], []) = raise EmptyQueue
      | dequeue (a, []) = 
          let val (h::t) = rev a
          in (h, ([], t))
          end
      | dequeue (a,(h::t)) = (h, (a, t))
end; *)
(* 
- open FunctionQueue;
opening FunctionQueue
  exception EmptyQueue
  type elem = string tree
  type queue
  val isEmpty : queue -> bool
  val newQueue : unit -> queue
  val enqueue : elem * queue -> queue
  val dequeue : queue -> elem * queue
*)

(* structure BFF = struct
    structure Q = FunctionalQueue
    fun bf t = 
        let 
          val emptyQueue = Q.newQueue()
          val queue = Q.enqueue (t, queue)
          fun loop(queue) = 
              let 
                val (data, (l, r)) = Q.dequeue queue
                val nextQueue = Q.enqueue (r, l)
              in 
                (case Q.dequeue queue of
                  (data, (l, r)) => 
                    data::loop(nextQueue)
                | (_, _) => loop(nextQueue)
                ) 
                
              end
              handle Q.EmptyQueue => nil
        in 
          loop(queue)
        end
end; *)
(* 
stdIn:208.18-210.37 Error: case object and rules do not agree [tycon mismatch]
  rule domain: elem * (elem * elem)
  object: elem * queue
  in expression:
    (case (Q.dequeue queue)
      of (data,(l,r)) =>
           (Q.enqueue (<exp>,<exp>); Q.enqueue (<exp>,<exp>);
            data :: loop <exp>)
       | (_,_) => loop ())
*)

(* BFF.bf (fromPreOrder "1(2(3()())(4()()))(5()())"); *)
(* 全くわからない *)



(* 単純なリストによって、string treeを要素とする待ち行列を作れ *)
(* pp.128-129 をstringにしてsignatureやればでは？？？ *)

structure SimpleSTQueue :> POLY_QUEUE where type elem = string tree = struct
    exception EmptyQueue
    type elem = string tree
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

structure BFSST = struct
    structure Q = SimpleSTQueue
    fun bf t = 
        let val queue = Q.newQueue()
            fun loop() = 
                (case Q.dequeue queue of
                    Node(data, l, r) => (Q.enqueue (l, queue); Q.enqueue(r, queue); data::loop())
                  | Empty => loop()
                ) handle Q.EmptyQueue => nil
        in (Q.enqueue (t, queue); loop() )
        end
end;

BFSST.bf (fromPreOrder "1(2(3()())(4()()))(5()())");
