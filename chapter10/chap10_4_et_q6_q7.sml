(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 10.4 Q10.6 Q10.7
*)
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

signature POLY_QUEUE = sig
    exception EmptyQueue
    type elem
    type queue
    val newQueue: unit -> queue
    val enqueue: (elem*queue) -> unit
    val dequeue: queue -> elem
end;

(* 再帰だと深さ優先探索は簡単に作れるが、幅優先探索はうまく実現できない。 *)

(* 文字列型データを含む2分木string treeのための待ち行列 *)
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

(* 幅優先探索を行うストラクチャBF *)
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

(* 問10.6 *)
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

BF.bf (fromPreOrder "1(2(3()())(4()()))(5()())");

(* expect:
実際には参照型を使った循環2重リンクリストであるが省略している
fromPreOrder "1(2(3()())(4()()))(5()())"
=> Node(1, Node(2,Node(3, Empty, Empty),Node(4, Empty, Empty)),Node(5, Empty, Empty))

Bf.bf Node(1, Node(2,Node(3, Empty, Empty),Node(4, Empty, Empty)),Node(5, Empty, Empty))
=> 
queue = (ref [], ref []) : queue
=> 
Q.enqueue Node(1, Node(2,Node(3, Empty, Empty),Node(4, Empty, Empty)),Node(5, Empty, Empty)) 
queue = (Node(1, Node(2,Node(3, Empty, Empty),Node(4, Empty, Empty)),Node(5, Empty, Empty)) , ref [])
=> loop()
=>
Q.dequeue queue =>
enqueue(Node(2,Node(3, Empty),Node(4, Empty)), queue)
enqueue(Node(5, Empty, Empty), queue)
1::loop()
=> 
queue = ((Node(2,Node(3, Empty, Empty),Node(4, Empty, Empty)::Node(5, Empty, Empty)), ref [])
Q.dequeue queue =>
queue = (ref[], Node(5, Empty, Empty)))
enqueue Node(3, Empty, Empty) 
qnqueue Node(4, Empty, Empty)
1::2::loop()
=>
queue = (Node(3, Empty, Empty)::Node(4, Empty, Empty, Node(5, Empty, Empty)))
Q.dequeue queue =>
queue = (Node(3, Empty, Empty)::Node(4, Empty, Empty), ref nil)
enqueu Empty
enqueu Empty
1::2::5::loop()
=>
queue = (Node(3, Empty, Empty)::Node(4, Empty, Empty)::Empty::Empty, ref nil)
Q.dequeue queue =>
queue = (ref nil, Node(4, Empty, Empty):Empty::Empty)
enqueue Empty
enqueue Empty
1::2::5::3::loop()
=>
queue = (Empty::Empty, Node(4, Empty, Empty)::Empty::Empty)
Q.dequeue queue =>
queue = (ref nil, Empty::Empty)
enqueue Empty
enqueue Empty
1::2::5::3::4::loop()
=> 
queue = (Empty::Empty::Empty::Empty, Empty::Empty)
Q.dequeue queue =>
queue = (Empty::Empty::Empty::Empty, Empty)
Empty => loop()
1::2::5::3::4::loop()
queue = (Empty::Empty::Empty::Empty, Empty)
Q.dequeue queue =>
queue = (Empty::Empty::Empty::Empty, ref nil)
Empty => loop()
1::2::5::3::4::loop()
...
queue = (ref nil, Empty)
Q.dequeue queue =>
queue = (ref nil, ref nil)
handle Q.EmptyQueue => nil
1::2::5::3::4::nil
[1,2,4,3,4]

result: 
val it = ["1","2","5","3","4"] : string list
*)

(* Q10.7 *)
(* signature POLY_QUEUE2 = sig
    exception EmptyQueue
    type elem
    type queue
    val newQueue: unit -> queue
    val enqueue: (elem*queue) -> unit
    val dequeue: queue -> elem
end *)

structure ITQueue :> POLY_QUEUE where type elem = int tree = struct
    exception EmptyQueue
    type elem = int tree
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

structure BFI = struct
    structure Q = ITQueue
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

fun fromIntPreOrder s =
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
     else let
            val (root, left, right) = decompose s
            val SOME data = Int.fromString(root)
          in Node(data, fromIntPreOrder left, fromIntPreOrder right)
          end
  end;

fromIntPreOrder "1(2(3()())(4()()))(5()())";
(* val it = Node (SOME 1,Node (SOME #,Node #,Node #),Node (SOME #,Empty,Empty))
  : int option tree 
Int.fromString : string -> int option
だから。
*)

BFI.bf (fromIntPreOrder "1(2(3()())(4()()))(5()())");
