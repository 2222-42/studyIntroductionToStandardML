(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 10.1 Q10.2 Q10.3
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
(* open IntQueue;
dequeue q; *)

(* 単純でエレガントな解決法:
関数型待ち行列
主要アイディア：
- 待ち行列Lを
  - 新しく先頭に加えられた要素のリストと
  - より以前に加えられた先に取り除かれる要素のリストに
- 分割し、待ち行列を以下のように実現する
*)


structure FastIntQueue = struct
    exception EmptyQueue
    (* newItemsとoldItemsに分割する。
    待ち行列全体はnewItemsとoldItemsの逆順リストとを連結したものに等しくなるようにする。 *)
    type queue = int list ref * int list ref
    (* 空の待ち行列を作成 *)
    fun newQueue() = (ref [], ref []) : queue
    (* 末尾に要素を追加。newItemsリストの先頭に要素を付け加える *)
    fun enqueue (i, (a,b)) = a := i :: (!a)
    (* 待ち行列の先頭の要素を取り除き、その要素を返す。 *)
    (* 
      - oldList が空でなければ
        - その先頭を取り除き
        - 取り除いた値を返す
      - oldItemsが空でかつnewItemsが空でなければ、
        - newItemsの逆順のリストを作成し、
          - それの先頭要素を取り除き、
          - 取り除いたリストを新たにoldItemsにし、
        - newItemsを空にし
        - 取り除いた値を返す
      - 両方からだったら、EmptyQueue例外を発生させる。
    *)
    fun dequeue (ref[], ref[]) = raise EmptyQueue
      | dequeue (a as ref L, b as ref []) = 
          let val (h::t) = rev L
          in (a:=nil; b:=t; h)
          end
      | dequeue (a,b as ref (h::t)) = (b := t; h)
end

(* Q10.2 *)
val q = FastIntQueue.newQueue();
astIntQueue.enqueue(0,q);
map (fn x => FastIntQueue.enqueue(x,q)) [1,3,5];
(* val it = (ref [5,3,1,0],ref []) : FastIntQueue.queue *)
FastIntQueue.dequeue q;
(* val it = (ref [],ref [1,3,5]) : FastIntQueue.queue *)
FastIntQueue.enqueue(7,q);
(* val it = (ref [7],ref [1,3,5]) : FastIntQueue.queue *)
(* val it = (ref [7],ref [3,5]) : FastIntQueue.queue *)
(* val it = (ref [7],ref [5]) : FastIntQueue.queue *)
(* val it = (ref [7],ref []) : FastIntQueue.queue *)

(* Q10.3 *)
(* enqueueとdequeueがランダムに行われる場合の、dequeueの平均の実行時間を見積もれ *)
(* 
    fun enqueue (i, (a,b)) = a := i :: (!a)
enqueueは
  cost 1?

    fun dequeue (ref[], ref[]) = raise EmptyQueue
      | dequeue (a as ref L, b as ref []) = 
          let val (h::t) = rev L
          in (a:=nil; b:=t; h)
          end
      | dequeue (a,b as ref (h::t)) = (b := t; h)

- oldList が空でなければ
  - その先頭を取り除き
    - cost 1
  - 取り除いた値を返す
    - cost 1 or 0?
- oldItemsが空でかつnewItemsが空でなければ、
  - newItemsの逆順のリストを作成し、
    - それの先頭要素を取り除き、
      - cost 1
    - 取り除いたリストを新たにoldItemsにし、
      - cost (n - 1) or 1?　
  - newItemsを空にし
    - cost 1
  - 取り除いた値を返す
    - cost 1 or 0?
- 両方からだったら、EmptyQueue例外を発生させる。
  - cost 1?

cost 1 or 2
cost (n + 2) or (n + 3) or 3 or 4
cost 1 or 2

enqueue の回数をp
dequeue の回数をqとする

常にp >= qとする。さもなくば、その時点でerrorが起きて、0回目の計算と同じになるから。
p + q*(1 + 3 + 1)/3
おおよそ、p+2q?
*)
