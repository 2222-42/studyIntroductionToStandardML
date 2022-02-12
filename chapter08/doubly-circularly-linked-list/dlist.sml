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
