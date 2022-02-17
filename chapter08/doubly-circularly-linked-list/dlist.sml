datatype 'a cell = NIL | CELL of {data: 'a, left: 'a cell ref, right: 'a cell ref}
type 'a dlist = 'a cell ref;
exception NilException

(*fun dataDlist (ref (CELL{data=d, ...})) = d;*)
fun dataDlist dlist =
    case dlist
     of ref (CELL {data=d, ...}) => d
      | ref NIL => raise NilException;


(*fun rightDlist (ref (CELL{right,...})) = right;*)
fun rightDlist dlist =
    case dlist
     of ref (CELL{right,...}) => right
      | ref NIL => raise NilException;

(*fun leftDlist (ref (CELL{left,...})) = left;*)
fun leftDlist dlist =
    case dlist
     of ref (CELL{left,...}) => left
      | ref NIL => raise NilException;

fun insertDlist a dlist =
    case dlist of
        ref (CELL{left=l1 as ref (CELL{right=r1,...}),...}) =>
        let val cell = CELL{
                    data=a,
                    right=ref(!dlist),
                    left=ref(!l1)}
            in (dlist:=cell; l1:=cell; r1:=cell)
            end
      | ref NIL =>
            let val l = ref NIL
                val r = ref NIL
                val cell = CELL{
                        data=a,
                        left=l,
                        right=r}
            in (dlist:=cell; l:=cell; r:=cell)
            end;

fun singleDlist a =
    let val l = ref NIL
        val r = ref NIL
        val cell = CELL{data=a, left=l, right=r}
    in (r:=cell; l:=cell; r)
    end;

fun deleteDlist dlist =
    case dlist of
        ref NIL => raise NilException
      | ref (CELL{left=l1 as ref (CELL{right=r2,left=l2,...}),
                   right=r1 as ref (CELL{right=r3,left=l3,...}),
                   ...}) =>
            if l1 = l2 then dlist := NIL
            else (dlist := !r1; r2 := !r1; l3 := !l1);

fun fromListToDlist list = foldr (fn (h, R) => (insertDlist h R; R)) (ref NIL) list;

fun concatDlist dlist1 dlist2 =
    case (dlist1, dlist2) of
            (ref NIL, _) => (dlist1 := !dlist2)
          | (_, ref NIL) => (dlist2 := !dlist1)
          | (d1 as ref (CELL {right=r1 as ref (CELL{right=r11,...}), left=l1, ...}),
            d2 as ref (CELL {right=r2 as ref (CELL{right=r21,...}), left=l2, ...}))
                => let
                    val previousL1 = !l1
                    val previousR11 = !r11
                   in
                    (l1 := !l2; l2 := previousL1; r11 := !r21; r21 := previousR11)
                   end;

(* すでに処理済みかどうかを判定 *)
fun member x visited =
    case visited
     of nil => false
      | (h::t) => if h = x then true else member x t;

fun dlistToList L =
    let fun f l visited = (*すでにたどったポインタをvisitedに記録しながらリストを右にたどっていく*)
        if member l visited then nil
        else (dataDlist l)::(f (rightDlist l) (l::visited))
    in f (rightDlist (leftDlist L)) nil (* 最初の要素の重複をさけるために、リンク内のポインタからスタート*)
    end;

(* mapDlist を使えばもっと簡単に定義できる *)
fun copyDlist DL =
    let val list = dlistToList DL
    in fromListToDlist list
    end;

fun mapDlist f d =
    let
        (* newElemを使って、終了条件を判定している。ただ真偽を判定するのではなく、結果に渡すべきDlistも出している。*)
        fun newElem x nil = NONE
          | newElem x ((h,newH)::t) =
            if x = h then SOME newH
            else newElem x t
        (*（元の参照, 新しい参照）の組を記録することによって与えられた参照構造と同じ構造をつくっている *)
        (* 与えられた参照が外部の参照の場合、その構造もコピーしたほうがよいことも満たしている。*)
        fun copy l copied =
            case l of
                ref NIL => ref NIL
              | ref (CELL{left, right, data}) =>
                    (case newElem l copied of
                         NONE =>
                             let
                                 val newL = ref NIL
                                 val copied = (l, newL)::copied
                                 val l = copy left copied
                                 val r = copy right copied
                             in
                                 (newL := CELL{left = l, right = r, data = f data};
                                  newL)
                             end
                       | SOME newL => newL
                    )
    in
        (* 以下のように、最初の要素の重複を避けるためにリンク内のポインタからスタートするようにしていない理由は、 *)
        (* (元の参照、新しい参照）の組を記録することによって与えられた参照構造と同じ構造をつくっているので、CELL内部のポインタから始めるという処理はいらないから *)
        copy d nil
    end;
