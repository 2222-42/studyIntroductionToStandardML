(* SML source file. Copyright (c) by 2222-42 2020.
* Q10.8
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

signature FUNCTIONAL_POLY_QUEUE = sig
    exception EmptyQueue
    type elem
    type queue
    val newQueue: unit -> queue
    val enqueue: (elem*queue) -> queue
    val dequeue: queue -> (elem*queue)
end;

structure FunctionalQueue :> FUNCTIONAL_POLY_QUEUE where type elem = string tree = struct
    exception EmptyQueue
    type elem = string tree
    type queue = elem list * elem list 
    fun newQueue() = ([], [])
    fun enqueue (i, (a,b)) = (i::a, b)
    fun dequeue ([], []) = raise EmptyQueue
      | dequeue (list, []) = 
          let val (h::t) = rev list
          in (h, ([], t))
          end
      | dequeue (list,(h::t)) = (h, (list, t))
end;
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

structure BFF = struct
    structure Q = FunctionalQueue
    fun bf t = 
      let 
        val emptyQueue = Q.newQueue()
        val firstQueue = Q.enqueue(t, emptyQueue)
        fun loop(queue) = 
          let
            val (target, q) = Q.dequeue(queue)
          in
            (case target of 
                Node(data, l, r) => data::(loop(Q.enqueue(r,Q.enqueue(l, q))))
              | Empty => loop(q)
            )handle Q.EmptyQueue => nil
          end
      in 
        loop(firstQueue)
      end
end;
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

BFF.bf (fromPreOrder "1(2(3()())(4()()))(5()())");
(* 全くわからない -> わかった。#25 *)

(* 筆者の回答例については、疑惑があるので、ここへの記載は省略。
理由は、関数型待ち行列の定義をどうするか、にあるからである。
-> p.130に「以下の説明では待ち行列を保持するために参照型を使用しているが，
 {\tt enqueue} や {\tt dequeue} が変更された待ち行列を返すような仕様に
 することにより，純粋に関数型言語の枠内で実現が可能である．
 この意味で，この手法は関数型待ち行列と呼ばれる．」と断りを記載されているので、筆者の解答例はある意味で正しい。
もちろん、回答者の解答はより関数型であるが。
*)


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
(* 筆者の解答は同じだったの省略 *)

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
  
(* 筆者の解答は同じだったの省略 *)

BFSST.bf (fromPreOrder "1(2(3()())(4()()))(5()())");
