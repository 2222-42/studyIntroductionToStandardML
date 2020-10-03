(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 10.5 Q10.10
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

functor QueueFUN(type elem) :> POLY_QUEUE where type elem = elem = struct
    exception EmptyQueue
    type elem = elem
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

(* 筆者の解答： 
  functor QueueFun(type ty) :> POLY_QUEUE where type elem = ty = 
...
としており、elemとtyとタイプの名前を変えているので、可読性があがっているようだ。
*)

structure ITQueue = QueueFUN(type elem = int tree);
structure STQueue = QueueFUN(type elem = string tree);

structure BF = struct
    structure Q = STQueue
    fun bffold f z t =
    let val queue = Q.newQueue()
        fun loop () =
            (case Q.dequeue queue of
                    Node(data,r,l) => (Q.enqueue (r,queue);
                                        Q.enqueue (l,queue);
                                        f (data, loop()))
                    | Empty => loop())
            handle Q.EmptyQueue => z
    in (Q.enqueue (t,queue); loop())
    end
    fun bf t = bffold (op ::) nil t
end;

structure BFI = struct
    structure Q = ITQueue
    fun bffold f z t =
    let val queue = Q.newQueue()
        fun loop () =
            (case Q.dequeue queue of
                    Node(data,r,l) => (Q.enqueue (r,queue);
                                        Q.enqueue (l,queue);
                                        f (data, loop()))
                    | Empty => loop())
            handle Q.EmptyQueue => z
    in (Q.enqueue (t,queue); loop())
    end
    fun bf t = bffold (op ::) nil t
end;
(* 筆者の解答:
   structure BF = struct
     structure Q = STQueue
     fun bf t =
       let val queue = Q.newQueue()
           fun loop () =
               (case Q.dequeue queue of
                      Node(data,r,l) => (Q.enqueue (r,queue);
                                         Q.enqueue (l,queue);
                                         data::loop())
                    | Empty => loop())
               handle Q.EmptyQueue => nil
       in (Q.enqueue (t,queue); loop())
       end
   end
bffoldを使っていないだけで回答に違いはない。
*)

(* check *)

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

val a = BF.bf (fromPreOrder "1(2(3()())(4()()))(5()())");
val b = BF.bffold (op ::) nil (fromPreOrder "1(2(3()())(4()()))(5()())");
val c = BF.bffold (op ^) "" (fromPreOrder "1(2(3()())(4()()))(5()())");
val d = foldr (op ^) "" (BF.bf (fromPreOrder "1(2(3()())(4()()))(5()())"));

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

val ai = BFI.bf (fromIntPreOrder "1(2(3()())(4()()))(5()())");
val bi = BFI.bffold (op ::) nil (fromIntPreOrder "1(2(3()())(4()()))(5()())");
(* End of Q10.10 *)


(* structure CQueue :> POLY_QUEUE where type elem = char = struct
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
end; *)

signature BUFFER = sig
  exception EndOfBuffer
  type channel
  val openBuffer :unit -> channel
  val input : channel -> char
  val output : channel * char -> unit
end

functor BufferFUN(structure CQueue : POLY_QUEUE where type elem = char) :> BUFFER = 
struct
  exception EndOfBuffer
  type channel = CQueue.queue
  fun openBuffer () = CQueue.newQueue()
  fun input ch = CQueue.dequeue ch
                 handle CQueue.EmptyQueue => raise EndOfBuffer
  fun output(ch,c) = CQueue.enqueue (c, ch)
end

structure CQueue = QueueFUN(type elem = char);
structure Buffer = BufferFUN(structure CQueue = CQueue);
