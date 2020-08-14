(* SML source file. Copyright (c) by 2222-42 2020.
* Q10.11
*)

signature FUNQUEUE = sig
    exception EmptyQueue
    type 'a queue
    val newQueue: unit -> 'a queue
    val enqueue: 'a * 'a queue -> 'a queue
    val dequeue: 'a queue -> 'a * 'a queue
end

signature BUFFER = sig
  exception EndOfBuffer
  type channel
  val openBuffer :unit -> channel
  val input : channel -> char
  val output : channel * char -> unit
end

(* 
FUNQUEUE シグネチャを持つストラクチャを受取、BUFFERシグネチャを持つストラクチャを生成するように
FUNQUEUEシグネチャは関数型で
BUFFER シグネチャは、関数型ではない
これだと、inputしてもoutputしても結果がわからなくなるのでは？
作問意図がよくわからない。

つまり、関数型を手続き型に変えろ？
*)
functor BufferFUN(structure FQueue : FUNQUEUE ) :> BUFFER = struct
  exception EndOfBuffer
  type channel = char FQueue.queue ref
  fun openBuffer () = ref (FQueue.newQueue()) : channel
  fun input ch = let
                    val (out, queue) = (FQueue.dequeue (!ch))
                 in
                    (ch := queue; out)
                 end handle FQueue.EmptyQueue => raise EndOfBuffer
  fun output(ch, c) = 
                let
                    val queue = FQueue.enqueue (c, !ch)
                in
                    ch := queue
                end 
end;
(* 
chapter10\chap10_5_q12.sml:30.9-38.4 Error: value type in structure does not match signature spec
    name: input
  spec:   ?.channel -> char
  actual: 'a ?.queue -> 'a * 'a ?.queue
chapter10\chap10_5_q12.sml:30.9-38.4 Error: value type in structure does not match signature spec
    name: output
  spec:   ?.channel * char -> unit
  actual: 'a ?.queue * 'a -> 'a ?.queue
*)
(* 
  type channel = char FQueue.queue 
chapter10\chap10_5_q12.sml:38.22-38.33 Error: operator and operand do not agree [tycon mismatch]
  operator domain: 'Z ref * 'Z
  operand:         'Y ?.queue * 'Y ?.queue
  in expression:
    ch := queue
chapter10\chap10_5_q12.sml:44.22-44.33 Error: operator and operand do not agree [tycon mismatch]
  operator domain: 'Z ref * 'Z
  operand:         'Y ?.queue * 'Y ?.queue
  in expression:
    ch := queue
*)

(* 筆者の解答:
   functor BufferFUN(structure FQueue : FUNQUEUE)
           :> BUFFER =
   struct
    exception EndOfBuffer
    type channel = char FQueue.queue
    fun openBuffer () = FQueue.newQueue()
    fun input ch = FQueue.dequeue ch
                   handle FQueue.EmptyQueue => raise EndOfBuffer
    fun output(ch,c) = FQueue.enqueue (ch,c)
   end
これだと、上述の通り、ストラクチャの値の型でspecとの不一致が起きる。
*)
