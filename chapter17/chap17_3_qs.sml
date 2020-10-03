(* SML source file. Copyright (c) by 2222-42 2020.
* Q17.5 Q17.6 Q17.7
*)

signature SORT = sig
    val sort : 'a array * ('a * 'a -> order) -> unit
end

structure ArrayQuickSortOpt : SORT  =
   struct
   local
     open Array
   in
     fun sort (array,comp) =
         let
           fun swap (i,j) =
               let val temp = sub(array,i)
               in (update(array,i,sub(array,j)); update(array,j,temp))
               end
           fun sort3 i =
               case comp(sub(array,i),sub(array,i+1))
               of GREATER =>
                 (case comp(sub(array,i+1),sub(array,i+2))
                  of GREATER => (* 3 2 1 *)
                     swap(i,i+2)
                   | _ => (case comp(sub(array,i),sub(array,i+2))
                           of GREATER => (* 3 1 2 *)
                              let val ei = sub(array,i) in
                                (update(array,i,sub(array,i+1));
                                 update(array,i+1,sub(array,i+2));
                                 update(array,i+2,ei))
                              end
                            | _ => (* 2 1 3 *)
                              (swap(i,i+1))
                          )
                 )
                | _ =>
                  (case comp(sub(array,i+1),sub(array,i+2))
                    of GREATER =>
                       (case comp(sub(array,i),sub(array,i+2))
                         of GREATER => (* 2 3 1 *)
                            let val ei = sub(array,i) in
                              (update(array,i,sub(array,i+2));
                               update(array,i+2,sub(array,i+1));
                               update(array,i+1,ei))
                            end
                          | _ => (* 1 3 2 *)
                            (swap(i+1,i+2))
                       )
                     | _ =>  (* 1 2 3 *)
                       ())
           fun getPivot (i,j) =
               let
                 val delta = (j-i) div 4
                 val i = i + delta
                 val j = i + delta * 3
                 val mid = i + (j-i) div 2
                 val ie = sub(array,i)
                 val je = sub(array,j)
                 val me = sub(array,mid)
               in
                 case (comp(ie,me),comp(me, je))
                  of (LESS, LESS) => (mid,me)
                   | (LESS, _) => (case comp(ie, je) of LESS => (j,je) | _ => (i,ie))
                   | (_, GREATER) => (mid,me)
                   | _ => (case comp(ie, je) of LESS => (i,ie) | _ => (j,je))
               end
           fun qsort (i,j) =
               if j < i+2 then ()
               else if j = i+2 then case (comp(sub(array,i),sub(array,i+1)))
                                    of GREATER => swap(i,i+1)
                                     | _ => ()
               else if j = i + 3 then sort3  i
               else
                 let
                   val pivot = if (j-i) < 30 then sub(array,i)
                               else
                                 let val (pi,pivot) = getPivot(i,j-1)
                                 in update(array,pi,sub(array,i));
                                    update(array,i,pivot);
                                    pivot
                                 end
                   fun partition (a,b) =
                       if b < a then a
                       else
                         let
                           fun scanRight a =
                               if a > b then a
                               else
                                 case comp(sub(array,a),pivot)
                                 of GREATER => a
                                  | _ => scanRight (a+1)
                           val a = scanRight a
                           fun scanLeft b =
                               if b < a then b
                               else
                                 case comp(sub(array,b),pivot)
                                 of GREATER => scanLeft (b - 1)
                                  | _ => b
                           val b = scanLeft b
                         in
                           if b < a then a
                           else (swap(a,b);
                                 partition (a+1,b-1))
                         end
                   val a = partition (i+1,j-1)
                   val _ =  swap(i,a-1)
                 in
                   (qsort (i,a-1); qsort (a,j))
                 end
         in
           qsort (0,Array.length array)
         end
   end
   end;




(* 
val test = randomList 50;

fun printer L = 
   map (fn x => print(Int.toString(x)^" "))L; 

printer test;   
*)


(* 
val test = "30 15 67 74 41 51 34 70 67 46 46 19 6 37 69 30 3 31 52 25 44 38 12 30 61 32 12 8 64 60 53 67 43 48 40 67 51 62 49 46 35 18 16 4 65 70 35 18 48 60";
*)
use "./chapter17/lexer.sml";
fun makeIntListFromFile inf = 
  let
    exception NotDigits
    val ins = TextIO.openIn inf;
    fun fromTokentoInt t = 
      case t of 
        DIGITS(s) => Int.fromString(s)
      | _ => raise NotDigits
    fun getIntList ins L = 
      (case fromTokentoInt (lex ins)of
         SOME i => (getIntList ins (L@[i]))
       | NONE => (getIntList ins L))
       handle NotDigits => L
  in
    getIntList ins [] before TextIO.closeIn ins
  end;

use "./chapter16/chap16_3_q9.sml";
fun makeSorttedOutputFile inf outf = 
  let
    val intArray = Array.fromList(makeIntListFromFile inf)
    fun copyArrayToOut array outf = 
      let
        val outs = TextIO.openOut outf
        val str = Array.foldr (fn (h, R) => Int.toString(h)^" "^R) "" array
      in
        TextIO.output (outs, str); TextIO.closeOut outs
      end
  in
    (Format.printf "Input file %s contains %d records.\n Now sorting ...." [Format.S inf, Format.I (Array.length(intArray))];
    ArrayQuickSortOpt.sort (intArray, Int.compare);
    copyArrayToOut intArray outf;
    Format.printf "done.\n" [])
  end
(*
ArrayQuickSortOpt.sort(target2, Int.compare);
val intArray = Array.fromList(makeIntListFromFile "./chapter17/test.txt");
fun toStringFromArray array = 
  Array.foldr (fn (h, R) => Int.toString(h)^" "^R) "" array;
makeSorttedOutputFile "./chapter17/test.txt" "./chapter17/outs.txt"
*)

(* Q17.6 *)
fun sort (x, [inf, outf]) = 
  (print(x^"\n");
   makeSorttedOutputFile inf outf;
   OS.Process.success)

(* SMLofNJ.exportFn("sort", sort); *)

(* sml @SMLload=sort.amd64-linux "./chapter17/test.txt" "outs.txt"  *)

(* Q17.7 *)

local val seed = (Random.rand(0,1))
in fun randomNat n = Random.randNat seed
end;

fun genArray n = Array.tabulate(n, randomNat);

fun generateRandomInt n outf = 
      let
        val outs = TextIO.openOut outf
        val array = genArray n
        val str = Array.foldr (fn (h, R) => Int.toString(h)^"\n"^R) "" array
      in
        TextIO.output (outs, str); TextIO.closeOut outs
      end

fun generate (x, [n, outf]) = 
  (print(x^"\n");
   (case Int.fromString n of
      SOME v => (generateRandomInt v outf;OS.Process.success)
    | NONE => OS.Process.failure)
  )
(* SMLofNJ.exportFn("generate", generate); *)

(* sml @SMLload=generate.amd64-linux 100 "testInts.txt"  *)

fun checkSorted inf = 
  let
    val intList = makeIntListFromFile inf
    fun compare L = foldl (fn (x, (i, b)) => (x, (x >= i) andalso b) ) (~1, true) L
    fun printResult L = 
      case (compare L) of
         (_, true) => print "Sorted!\n"
        | _ => print "not sorted...\n"
  in
    (Format.printf "Input file %s contains %d records.\n Now checkin ....\n" [Format.S inf, Format.I (List.length(intList))];
    printResult intList
    )
  end

fun check (x, [inf]) = 
  (print(x^"\n");
   checkSorted inf;
   OS.Process.success
  )

(* SMLofNJ.exportFn("check", check); *)

(* sml @SMLload=check.amd64-linux  "testOuts.txt"  *)