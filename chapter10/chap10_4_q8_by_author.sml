datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

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
     fun newQueue () = (ref [],ref []) : queue
     fun enqueue (i,(a,b))  = a := i :: (!a)
     fun dequeue (ref [],ref []) = raise EmptyQueue
       | dequeue (a as ref L, b as ref []) =
           let val (h::t) = rev L
           in (a:=nil; b:=t; h)
           end
       | dequeue (a,b as ref (h::t)) = (b := t; h)
   end

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