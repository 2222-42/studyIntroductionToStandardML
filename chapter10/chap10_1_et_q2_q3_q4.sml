(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 10.1 Q10.2 Q10.3 Q10.4
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
FastIntQueue.enqueue(0,q);
map (fn x => FastIntQueue.enqueue(x,q)) [1,3,5];
(* val it = (ref [5,3,1,0],ref []) : FastIntQueue.queue *)
FastIntQueue.dequeue q;
(* val it = (ref [],ref [1,3,5]) : FastIntQueue.queue *)
FastIntQueue.enqueue(7,q);
(* val it = (ref [7],ref [1,3,5]) : FastIntQueue.queue *)
(* val it = (ref [7],ref [3,5]) : FastIntQueue.queue *)
(* val it = (ref [7],ref [5]) : FastIntQueue.queue *)
(* val it = (ref [7],ref []) : FastIntQueue.queue *)

(* 筆者の解答:
空の待ち行列から始めてenqueue(Q, e_i)をn回、 dequeueをm(n>m)実施した場合を考える。
 単純なリストの場合、リストの中身は、 
[e_n,e_{n−1},\<dots>,e_{n−m}](n>m))で あり、次のdequeue操作で、e_{n−m}が返される
任意のn,m(n>m)について、FastQueueの場合も、この状 態と同等の状態が保たれることが示せればよい。

FastIntQueueの操作途中の状態は、２つのリストの組(L1,L2)である。この２つのリストの中の要素を

L1=[e_1^1,\<dots>,e_n^1]
L2=[e_1^2,\<dots>,e_n^2]
とする。 
すると、

[e_1^1,\<dots>,e_n^1,e_n^2,\<dots>,e_1^2]=[e_n,e_{n−1},\<dots>,e_{n−m}]
が成立し、次に返される値e_1^2は、e_{n−m}であり、 リストを用いた待ち行列と同一である。 

この性質が、任意のn,m(n>m)で成り立つから、 FastQueueは単純なリストを用いた実装と同一の振る舞いをする。
*)

(* 回答者のコメント:
回答者のオリジナルの回答だと、これはただ振舞いだけを示しており、同一であることを明確に示していない。
*)

(* Q10.3 *)
(* enqueueとdequeueがランダムに行われる場合の、dequeueの平均の実行時間を見積もれ *)
(* 
    fun enqueue (i, (a,b)) = a := i :: (!a)
enqueueは
  cost 1?
    -> 1

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
      -> 0
- oldItemsが空でかつnewItemsが空でなければ、
  - newItemsの逆順のリストを作成し、
    -> n + 1
    - それの先頭要素を取り除き、
      - cost 1
    - 取り除いたリストを新たにoldItemsにし、
      - cost (n - 1) or 1?　
        -> n
  - newItemsを空にし
  - 取り除いた値を返す
    - cost 1 or 0?
      -> 0
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
-> まぐれで正解している。
*)

(* 筆者の解答：
dequeueに掛かる実行時間を
1. リストに要素を１つ追加するための必要な時間CONS
2. リストから先頭要素を１つ取り出すのに必要な時間CAR
の回数で見積もる。

L2 が空でない場合は
CAR=1,
CONS=0である。 

L2が空の場合、
L1の長さをNとすると、 
CAR=N+1,
CONS=Nに等しく、 これら回数の平均を見積もることである

この問題は、amortize cost（償却原価）の考え方を使えば、統計的・解析的な計算をせず、 即座に求めることができる。

dequeueで取り除かれる要素eに着目する。 
この要素は、enqueueでL1のリストの先頭に追加され、 
さらに、dequeue操作でとりだされる前のいずれかのdequeue操作で L1のリストから取り除かれ、L2のリストの先頭に追加される。 
このコストはCAR=1,CONS=1である

そこで、このeが待ち行列に追加された時、この要素を dequeueするための将来必要なコスト原価として計上しておく、と考える

すると、dequeue操作では、L2が空の時に、L1から L2へ移動させるコストはすでにこの原価に含まれている、と考え、 
取り除かれる要素eのコストは、eを取り除くための原価(CAR=1,CONS=1) ＋実際に掛かるコスト（CAR=1）と計算できる。 

以上から、dequeue操作の平均時間はCAR=2,CONS=1である
*)
(* 回答者のコメント: シンプルにかつクリアに考えれば楽に済む話だった。
取り除かれる要素eに着目して、議論すれば、もっと明確な回答が得られていたであろう。
*)


(* Q10.4 *)

(* local *)
  (* structure Q = IntQueue *)
  structure Q = FastIntQueue
(* in *)
(* stdIn:40.1-41.6 Error: syntax error: deleting  END LOCAL *)
  val testq = Q.newQueue();
  map (fn x => Q.enqueue(x,testq)) [1,3,5];
  Q.dequeue testq;
  testq;
(* end *)
(* 回答者補足: 
- local in endであると、参照できない？？？
- let in endでも参照できない。
-> 記述の仕方の間違え。
*)

local
  (* structure Q = IntQueue *)
  structure Q = FastIntQueue
in
  val test2 = Q.newQueue();
  val result = (map (fn x => Q.enqueue(x,test2)) [1,3,5]; Q.dequeue test2; test2)
end;

