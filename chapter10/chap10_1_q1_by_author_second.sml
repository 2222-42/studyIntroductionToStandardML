fun member x list = case list of nil => false
                            | (h::t) => if h = x then true else member x t;

   structure DList =
   struct
     datatype 'a cell
       = NIL
       | CELL of {data:'a, left:'a cell ref, right:'a cell ref}
     exception EMPTY_DLIST
     type 'a dlist = 'a cell ref
     fun emptyDlist () = ref NIL
     fun rightDlist (ref (CELL{right,...})) = right | rightDlist _ = raise EMPTY_DLIST
     fun leftDlist (ref (CELL{left,...})) = left | leftDlist _ = raise EMPTY_DLIST
     fun dataDlist (ref (CELL{data,...})) = data | dataDlist _ = raise EMPTY_DLIST
     fun singleton a =
         let
           val l = ref NIL
           val r = ref NIL
           val c = CELL{left=l, right=r, data=a}
         in  (l:=c; r:=c; ref c)
         end
     fun member x nil = false
       | member x (h::t) = x = h orelse member x t
     fun insert a dlist =
         case dlist of
           ref (CELL{left=l1 as ref (CELL{right=r1,...}),...}) =>
           let val newcell = CELL{data=a,
                                  right=ref (!dlist),
                                  left=ref (!l1)}
           in (dlist:=newcell; l1:=newcell; r1:=newcell)
           end
         | ref NIL =>
           let
             val l = ref NIL
             val r = ref NIL
             val cell = CELL{data=a,left=l,right=r}
           in  (dlist:=cell; l:=cell; r:=cell)
           end
         | _ => raise EMPTY_DLIST
   fun deleteDlist dlist =
       case dlist of
         ref NIL => ()
       | ref (CELL{left=l1 as ref (CELL{right=r2,left=l2,...}),
                   right=r1 as ref (CELL{right=r3,left=l3,...}),
                   ...}) =>
           if l1 = l2 then dlist := NIL
           else (dlist := !r1; r2 := !r1; l3 := !l1)
     fun toList L =
         let fun f l visited =
                 if member l visited then nil
                 else (dataDlist l)::(f (rightDlist l) (l::visited))
         in f (rightDlist (leftDlist L)) nil
         end
     fun fromList (L:int list) = foldl (fn (x,y) => (insert x y;y)) (ref NIL) L
     fun concatDlist D1 D2 =
         case (D1, D2) of
           (ref NIL, _) => D1 := !D2
         | (_, ref NIL) => D2 := !D1
         | (ref (CELL{left=d1l as ref (CELL{right=d1lr,...}),...}),
            ref (CELL{left=d2l as ref (CELL{right=d2lr,...}),...})) =>
           let
             val d1lCell = !d1l
             val d1lrCell = !d1lr
           in
             (d1l := !d2l;
              d1lr := !d2lr;
              d2l := d1lCell;
              d2lr := d1lrCell)
           end
         | _ => raise EMPTY_DLIST
     fun mapDlist f d =
         let
           fun newElem x nil = NONE
             | newElem x ((h,newH)::t) =
               if x = h then SOME newH
               else newElem x t
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
           copy d nil
         end
     fun copyDlist d = mapDlist (fn x => x) d
     fun foldrDlist F z d =
         let
           fun f d z visited =
               if member d visited then z
               else F (dataDlist d, f (rightDlist d) z (d::visited))
         in f (rightDlist (leftDlist d)) z nil
         end
     fun foldlDlist F z d =
         let
           fun f d z visited =
               if member d visited then z
               else f (rightDlist d) (F (dataDlist d, z)) (d::visited)
         in f (rightDlist (leftDlist d)) z nil
         end
   end   structure DList =
   struct
     datatype 'a cell
       = NIL
       | CELL of {data:'a, left:'a cell ref, right:'a cell ref}
     exception EMPTY_DLIST
     type 'a dlist = 'a cell ref
     fun emptyDlist () = ref NIL
     fun rightDlist (ref (CELL{right,...})) = right | rightDlist _ = raise EMPTY_DLIST
     fun leftDlist (ref (CELL{left,...})) = left | leftDlist _ = raise EMPTY_DLIST
     fun dataDlist (ref (CELL{data,...})) = data | dataDlist _ = raise EMPTY_DLIST
     fun singleton a =
         let
           val l = ref NIL
           val r = ref NIL
           val c = CELL{left=l, right=r, data=a}
         in  (l:=c; r:=c; ref c)
         end
     fun member x nil = false
       | member x (h::t) = x = h orelse member x t
     fun insert a dlist =
         case dlist of
           ref (CELL{left=l1 as ref (CELL{right=r1,...}),...}) =>
           let val newcell = CELL{data=a,
                                  right=ref (!dlist),
                                  left=ref (!l1)}
           in (dlist:=newcell; l1:=newcell; r1:=newcell)
           end
         | ref NIL =>
           let
             val l = ref NIL
             val r = ref NIL
             val cell = CELL{data=a,left=l,right=r}
           in  (dlist:=cell; l:=cell; r:=cell)
           end
         | _ => raise EMPTY_DLIST
   fun deleteDlist dlist =
       case dlist of
         ref NIL => ()
       | ref (CELL{left=l1 as ref (CELL{right=r2,left=l2,...}),
                   right=r1 as ref (CELL{right=r3,left=l3,...}),
                   ...}) =>
           if l1 = l2 then dlist := NIL
           else (dlist := !r1; r2 := !r1; l3 := !l1)
     fun toList L =
         let fun f l visited =
                 if member l visited then nil
                 else (dataDlist l)::(f (rightDlist l) (l::visited))
         in f (rightDlist (leftDlist L)) nil
         end
     fun fromList (L:int list) = foldl (fn (x,y) => (insert x y;y)) (ref NIL) L
     fun concatDlist D1 D2 =
         case (D1, D2) of
           (ref NIL, _) => D1 := !D2
         | (_, ref NIL) => D2 := !D1
         | (ref (CELL{left=d1l as ref (CELL{right=d1lr,...}),...}),
            ref (CELL{left=d2l as ref (CELL{right=d2lr,...}),...})) =>
           let
             val d1lCell = !d1l
             val d1lrCell = !d1lr
           in
             (d1l := !d2l;
              d1lr := !d2lr;
              d2l := d1lCell;
              d2lr := d1lrCell)
           end
         | _ => raise EMPTY_DLIST
     fun mapDlist f d =
         let
           fun newElem x nil = NONE
             | newElem x ((h,newH)::t) =
               if x = h then SOME newH
               else newElem x t
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
           copy d nil
         end
     fun copyDlist d = mapDlist (fn x => x) d
     fun foldrDlist F z d =
         let
           fun f d z visited =
               if member d visited then z
               else F (dataDlist d, f (rightDlist d) z (d::visited))
         in f (rightDlist (leftDlist d)) z nil
         end
     fun foldlDlist F z d =
         let
           fun f d z visited =
               if member d visited then z
               else f (rightDlist d) (F (dataDlist d, z)) (d::visited)
         in f (rightDlist (leftDlist d)) z nil
         end
   end                            

structure ImperativeIntQueue =
   struct
     exception EmptyQueue
     type queue = int DList.dlist
     fun newQueue() = DList.emptyDlist() : queue
     fun enqueue (item,queue) = DList.insert item queue
     fun dequeue queue =
         let
           val last = DList.leftDlist queue
           val data = DList.dataDlist last
         in
           (DList.deleteDlist last; data)
         end
         handle DList.EMPTY_DLIST => raise EmptyQueue
   end;

   