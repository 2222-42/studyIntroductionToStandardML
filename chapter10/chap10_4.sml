(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 10.4
*)
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

signature POLY_QUEUE = sig
    exception EmptyQueue
    type elem
    type queue
    val newQueue: unit -> queue
    val enqueue: (elem*queue) -> unit
    val dequeue: queue -> elem
end

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

(* 幅優先探索を行うスラクチャBF *)
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
end
