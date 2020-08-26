(* SML source file. Copyright (c) by 2222-42 2020.
* Chap14.1 Q14.1 Q14.2
*)

 Time.toString(Time.now());

(* 
signature DATE =
  sig
    datatype weekday = Fri | Mon | Sat | Sun | Thu | Tue | Wed
    datatype month
      = Apr | Aug | Dec | Feb | Jan | Jul | Jun | Mar | May | Nov | Oct | Sep
    type date
    exception Date
    val year : date -> int
    val month : date -> month
    val day : date -> int
    val hour : date -> int
    val minute : date -> int
    val second : date -> int
    val weekDay : date -> weekday
    val yearDay : date -> int
    val isDst : date -> bool option
    val offset : date -> Time.time option
    val localOffset : unit -> Time.time
    val
        date : {day:int, hour:int, minute:int, month:month,
                 offset:Time.time option, second:int, year:int}
                -> date
    val fromTimeLocal : Time.time -> date
    val fromTimeUniv : Time.time -> date
    val toTime : date -> Time.time
    val toString : date -> string
    val fmt : string -> date -> string
    val fromString : string -> date option
    val scan : (char,'a) StringCvt.reader -> (date,'a) StringCvt.reader
    val compare : date * date -> order
  end
*)

fun currentTime() = 
    Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal(Time.now()));
(*   DATE
    {day=21,hour=8,isDst=SOME false,minute=29,month=Aug,
     offset=SOME (TIME {usec=578254988000000}),second=42,wday=Fri,yday=234,
     year=2020} : Date.date *)
currentTime(); 
(* 筆者の解答はfmtかけずにそのまんま *)
(* end Q14.1 *)

(* signature TIMER =
  sig
    type cpu_timer
    type real_timer
    val startCPUTimer : unit -> cpu_timer
    val totalCPUTimer : unit -> cpu_timer
    val
        checkCPUTimes : cpu_timer
                         -> {gc:{sys:Time.time, usr:Time.time},
                          nongc:{sys:Time.time, usr:Time.time}}
    val checkCPUTimer : cpu_timer -> {sys:Time.time, usr:Time.time}
    val checkGCTime : cpu_timer -> Time.time
    val startRealTimer : unit -> real_timer
    val totalRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time
  end
*)

fun timeRun f x =
    let 
        val timer = Timer.startCPUTimer()
        val _ = f x
        val tm = Timer.checkCPUTimer timer
        val ut = Time.toMicroseconds (#usr tm)
    in LargeInt.toInt ut
    end;

local val seed = (Random.rand(0,1))
in fun randomInt n = Random.randInt seed
end;

fun genArray n = Array.tabulate(n, randomInt);

use "./chapter13/chap13_2_q4.sml";

fun convBase f r (x: real) = f x / f r;

(* Q14.2 *)
fun nlogn n = 
  (Real.fromInt n) * (convBase Math.log10 2.0 (Real.fromInt n));
(* 筆者の解答:
   fun log2 x = Math.log10 x / (Math.log10 2.0)
   fun nlogn n = ((Real.fromInt n) * (log2 (Real.fromInt n)))

convBaseを改めて作っているだけ。
*)

fun checkTime n = 
  let 
    val array = genArray n
    val tm = timeRun ArrayQuickSort.sort (array, Int.compare)
    val nlognRatio = Real.fromInt(tm) / (nlogn n)
  in
    (n, tm div 1000, nlognRatio)
  end
(* chapter14\chap14_et_q1.sml:94.9-94.36 Error: operator and operand do not agree [tycon mismatch]
  operator domain: real * real
  operand:         int * real
  in expression:
    tm / nlogn n *)
(* val checkTime = fn : int -> int * int * real *)

(* 筆者の解答は同じ *)
(* end Q14.2 *)

(* Q14.3 --start *)
fun testSort n = 
  let 
    fun printResult (n, tm, ratio) = 
      print ("size="^Int.toString(n)^", milli-secs= "^Int.toString(tm)^", micro-secs/n log(n)="^Real.toString(ratio)^"\n")
  in 
    printResult(checkTime n)
  end;

(* 筆者の解答では、Dynamic.ppという以下のような汎用の清書関数を使って
   # Dynamic.pp;
   val it = fn : [’a#reify. ’a -> unit]
、次のように定義している:
   fun printResult (n, tm, ratio) =
       Dynamic.pp
        {"size" = n, "milli-secs" = tm, "micro-secs/n long(n)" = ratio}
   fun testSort n = printResult (checkTime n)
。文字列に変換してくれているようなので、確かに便利そう。
SMLではDYNAMICなるものはなさそうなので、SML#で実行して確かめるしかない？
*)
(* --end Q14.3 *)

(* Q14.4 --start *)
fun padString str = 
  if String.size(str) < 20 then 
    let
      val length = 20 - String.size(str)
      fun addEmp (length, string) = 
        if length <= 0 then string
        else addEmp(length - 1, " "^string)
    in
      addEmp(length, str)
    end
  else str;

fun formatReal r = (Real.fmt (StringCvt.FIX (SOME 8)) r)
fun printLine (i1, i2, r) =
  print(padString(Int.toString(i1))
        ^padString(Int.toString(i2))
        ^padString(formatReal r)
        ^"\n");

fun evalSort list = 
  let 
    val results = map checkTime list
    val average = (foldr(fn ((a,b,c),R) => c+R ) 0.0 results)/Real.fromInt(length list)
  in
    print(padString("array size")
         ^padString("milli-sec.")
         ^padString("micro s./(n log(n))")
         ^"\n");
    map printLine results;
    print("---------------------------------------------------------------\n");
    print(padString(" ")
         ^padString("average")
         ^padString(formatReal(average))
         ^"\n")
  end;

(* 筆者の解答: 
- padStringを定義する代わりに、StringCvtストラクチャの padLeft関数を使用している。
- 小数点以下8桁を表示するようにfmtを使っている
*)
fun evalSortByAuthor L =
    let
      val L' = map checkTime L
      val av = (foldr (fn ((_,_,x),y) => y+x) 0.0 L')/(Real.fromInt (List.length L'))
      val title = (StringCvt.padLeft #" " 20 "array size")
                  ^ (StringCvt.padLeft #" " 20 "milli-sec.")
                  ^ (StringCvt.padLeft #" " 20 "micro s./nlogn")
                  ^ "\n"
      fun formatReal a = StringCvt.padLeft #" " 20 (Real.fmt (StringCvt.FIX (SOME 8)) a)
      fun printLine (n,a,c) =
          let
            val ns =  StringCvt.padLeft #" " 20 (Int.toString n)
            val sa =  StringCvt.padLeft #" " 20 (Int.toString a)
            val sc = formatReal c
          in
            print (ns ^ sa ^ sc ^ "\n")
          end
    in
      (print title;
        map printLine L';
        print "------------------------------------------------------------\n";
        print ("                                 avarage" ^ (formatReal av));
        print "\n")
    end

val test_list = [10000,100000,1000000];
(* evalSort test_list; *)
(* --end Q14.4 *)

(* --start Q14.5 *)
fun eval {prog, input, size, base} =
  let
    val tm = timeRun prog input
    val (n: int) = size input
    val ratio = Real.fromInt tm / base n
  in
    (n, tm div 1000, ratio)
  end;
(* val eval = fn
  : {base:'a -> real, input:'b, prog:'b -> 'c, size:'b -> 'a}
     -> 'a * int * real *)
(* val eval = fn
  : {base:int -> real, input:'a, prog:'a -> 'b, size:'a -> int}
     -> int * int * real *)

(* eval {prog=(ArrayQuickSort.sort:int array * (int * int -> order) -> unit), input=(test_list, Int.compare), size=(length:int list -> int), base=nlogn};
ArrayQuickSort.sortとは型がどうしても一致しない。あくまでも汎用ケース。
*)
(* 筆者の解答は同じなので省略 *)
(* --end Q14.5 *)


(* Q14.6 *)


fun checkTimePerCompare n = 
  let 
    val array = genArray n
    fun compSub array = 
      let 
        val p = Array.sub(array, 0)
        val m = n div 2
        fun compare x = if x <= m then ()
                        else (Int.compare(Array.sub(array, n - x), p);
                              Int.compare(Array.sub(array, x - 1), p);
                              compare(x-1)
                              )
      in
        compare n
      end
  in
    eval {prog=compSub, input=array, size=Array.length, base=real}
  end
(* fun checkTimePerCompare n = 
  let 
    (* n入れて、arrayにして、またnにしているから無駄がある。 *)
    val array = genArray n
    fun compareElementsOfArray array = 
      let 
        val n = Array.length(array)
        val p = Array.sub(array, 0)
        val m = n div 2
        fun compare x = if x <= m then ()
                        else (Int.compare(Array.sub(array, n - x), p);
                              Int.compare(Array.sub(array, x - 1), p);
                              compare(x-1)
                              )
      in
        compare n
      end
  in
    eval {prog=compareElementsOfArray, input=array, size=Array.length, base=real}
  end 

上記のだと実行時間がとても少ないがなぜだろうか
- evalCompare test_list;
          array size          milli-sec.         micro s./h)
              500000                   5          0.01000000
             1000000                   8          0.00800000
             5000000                  59          0.01180000
---------------------------------------------------------------
                                 average          0.00993333  
*)

fun evalCompare list =
  let 
    val results = map checkTimePerCompare list
    val average = (foldr(fn ((_,_,c),R) => c+R ) 0.0 results)/Real.fromInt(length list)
    fun formatReal a = StringCvt.padLeft #" " 20 (Real.fmt (StringCvt.FIX (SOME 8)) a)
  in
    print(padString("array size")
         ^padString("milli-sec.")
         ^padString("micro s./n)")^"\n");
    map printLine results;
    print("---------------------------------------------------------------\n");
    print(padString(" ")
         ^padString("average")
         ^padString(formatReal average)^"\n")
  end;

val test_list = [500000,1000000,5000000];
(* evalCompare test_list; *)

(* 筆者の解答 *)
fun evalCompareByAuthor L =
    let
      fun comp n =
          let
            val array = genArray n
            val p = Array.sub(array, 0)
            val m = n div 2
            fun  loop x = if x <= m then ()
                          else (Int.compare (Array.sub(array, n - x), p);
                                Int.compare (Array.sub(array, x - 1), p);
                                loop (x -1))
          in
            loop n
          end
      fun evalN n = eval {prog = comp, input = n, size = fn x => x, base = real}
      val L' = map evalN L
      val av = (foldr (fn ((_,_,x),y) => y+x) 0.0 L')/(Real.fromInt (List.length L'))
      val title = (StringCvt.padLeft #" " 20 "array size")
                  ^ (StringCvt.padLeft #" " 20 "milli-sec.")
                  ^ (StringCvt.padLeft #" " 20 "micro s./n")
                  ^ "\n"
      fun formatReal a = StringCvt.padLeft #" " 20 (Real.fmt (StringCvt.FIX (SOME 8)) a)
      fun printLine (n,a,c) =
          let
            val ns =  StringCvt.padLeft #" " 20 (Int.toString n)
            val sa =  StringCvt.padLeft #" " 20 (Int.toString a)
            val sc = formatReal c
          in
            print (ns ^ sa ^ sc ^ "\n")
          end
    in
      (print title;
      map printLine L';
      print "------------------------------------------------------------\n";
      print ("                                 avarage" ^ (formatReal av));
      print "\n")
    end
(* evalCompareByAuthor test_list; *)

fun evalCompareModified L =
    let
      fun comp array =
          let
            val n = Array.length(array)
            val p = Array.sub(array, 0)
            val m = n div 2
            fun  loop x = if x <= m then ()
                          else (Int.compare (Array.sub(array, n - x), p);
                                Int.compare (Array.sub(array, x - 1), p);
                                loop (x -1))
          in
            loop n
          end
      fun evalN n = 
        let val array = genArray n
        in eval {prog = comp, input = array, size = Array.length, base = real}
        end
      val L' = map evalN L
      val av = (foldr (fn ((_,_,x),y) => y+x) 0.0 L')/(Real.fromInt (List.length L'))
      val title = (StringCvt.padLeft #" " 20 "array size")
                  ^ (StringCvt.padLeft #" " 20 "milli-sec.")
                  ^ (StringCvt.padLeft #" " 20 "micro s./n")
                  ^ "\n"
      fun formatReal a = StringCvt.padLeft #" " 20 (Real.fmt (StringCvt.FIX (SOME 8)) a)
      fun printLine (n,a,c) =
          let
            val ns =  StringCvt.padLeft #" " 20 (Int.toString n)
            val sa =  StringCvt.padLeft #" " 20 (Int.toString a)
            val sc = formatReal c
          in
            print (ns ^ sa ^ sc ^ "\n")
          end
    in
      (print title;
      map printLine L';
      print "------------------------------------------------------------\n";
      print ("                                 avarage" ^ (formatReal av));
      print "\n")
    end

(* evalCompareModified test_list; *)

(* Q14.7 *)

fun normalEvalSort list = 
  let 
    val compareResults = map checkTimePerCompare list
    val constant = (foldr(fn ((_,_,c),R) => c+R ) 0.0 compareResults)/Real.fromInt(length list)
    (* eval使った方がいい *)
    fun sort n = 
      let 
        val array = genArray n
      in
        ArrayQuickSort.sort (array, Int.compare)
      end
    fun base n = constant * nlogn n
    fun evalN n = eval {prog = sort, input = n, size = fn x => x, base = base}
    val results = map evalN list
    val average = (foldr(fn ((a,b,c),R) => c+R ) 0.0 results)/Real.fromInt(length list)
    fun formatReal a = StringCvt.padLeft #" " 20 (Real.fmt (StringCvt.FIX (SOME 8)) a)
  in
    print(padString("array size")^padString("time in cunit")^padString("T/(n log(n))")^"\n");
    map printLine results;
    print("---------------------------------------------------------------\n");
    print(padString(" ")^padString("average")^padString(formatReal average)^"\n");
    print("The estimated sort time function: T(n) = "^(Real.fmt (StringCvt.FIX (SOME 2)) average)^" n log (n)\n")
  end;

(* 
val normalEvalSort = fn : int list -> unit
- normalEvalSort test_list;
          array size       time in cunit        T/(n log(n))
              500000                 226       3.58131972804
             1000000                 476        3.5822569484
             5000000                2702       3.64257362441
---------------------------------------------------------------
                                 average       3.60205010028

処理性能が桁で変わったぞ、何だ何が変わったんだ？-> NormalEvalSortよりも、他のcheckTimePerCompare の変更の影響が大きいことがわかった。
- normalEvalSort test_list;
          array size       time in cunit        T/(n log(n))
              500000                 319          0.38676730
             1000000                 652          0.37542379
             5000000                3922          0.40453397
---------------------------------------------------------------
                                 average          0.38890835
The estimated sort time function: T(n) = 0.39 n log (n)
*)

(* 筆者の解答: *)
   fun evalCompareN L =
       let
         fun comp n =
             let
               val array = genArray n
               val p = Array.sub(array, 0)
               val m = n div 2
               fun  loop x = if x <= m then ()
                             else (Int.compare (Array.sub(array, n - x), p);
                                   Int.compare (Array.sub(array, x - 1), p);
                                   loop (x -1))
             in
               loop n
             end
         fun evalN n = eval {prog = comp, input = n, size = fn x => x, base = real}
         val L' = map evalN L
         val av = (foldr (fn ((_,_,x),y) => y+x) 0.0 L')/(Real.fromInt (List.length L'))
       in
         av
       end
   fun evalSortN c L =
       let
         fun sort n =
             let
               val array = genArray n
             in
               ArrayQuickSort.sort (array, Int.compare)
             end
         fun base n = c * nlogn n
         fun evalN n = eval {prog = sort, input = n, size = fn x => x, base = base}
         val L' = map evalN L
         val av = (foldr (fn ((_,_,x),y) => y+x) 0.0 L') / (Real.fromInt (List.length L'))
         val title = (StringCvt.padLeft #" " 20 "array size")
                     ^ (StringCvt.padLeft #" " 20 "time in cunit")
                  ^ (StringCvt.padLeft #" " 20 "C/nlogn")
                  ^ "\n"
         fun formatReal a = StringCvt.padLeft #" " 20 (Real.fmt (StringCvt.FIX (SOME 8)) a)
         fun printLine (n,a,c) =
            let
              val ns =  StringCvt.padLeft #" " 20 (Int.toString n)
              val sa =  StringCvt.padLeft #" " 20 (Int.toString (Real.floor (real a * 1000.0/ c)))
              val sc = formatReal c
            in
              print (ns ^ sa ^ sc ^ "\n")
            end
         val C = Real.fmt (StringCvt.FIX (SOME 2)) av
       in
         (print title;
          map printLine L';
          print "------------------------------------------------------------\n";
          print ("                                 avarage" ^ (formatReal av) ^ "\n");
          print ("The estimated sort time function: T(n) = " ^ C ^ " n log(n)\n"))
       end
   fun normalEvalSortByAuthor L = evalSortN (evalCompareN L) L

(* 筆者の解答の修正 *)

   fun evalCompareNModified L =
       let
         fun comp array =
             let
               val n = Array.length(array)
               val p = Array.sub(array, 0)
               val m = n div 2
               fun  loop x = if x <= m then ()
                             else (Int.compare (Array.sub(array, n - x), p);
                                   Int.compare (Array.sub(array, x - 1), p);
                                   loop (x -1))
             in
               loop n
             end
        fun evalN n = 
          let val array = genArray n
          in eval {prog = comp, input = array, size = Array.length, base = real}
          end
         val L' = map evalN L
         val av = (foldr (fn ((_,_,x),y) => y+x) 0.0 L')/(Real.fromInt (List.length L'))
       in
         av
       end
   fun evalSortNModified c L =
       let
         fun sort array =
               ArrayQuickSort.sort (array, Int.compare)
         fun base n = c * nlogn n
         fun evalN n = 
           let val array = genArray n
           in eval {prog = sort, input = array, size = Array.length, base = base}
           end
         val L' = map evalN L
         val av = (foldr (fn ((_,_,x),y) => y+x) 0.0 L') / (Real.fromInt (List.length L'))
         val title = (StringCvt.padLeft #" " 20 "array size")
                     ^ (StringCvt.padLeft #" " 20 "time in cunit")
                  ^ (StringCvt.padLeft #" " 20 "C/nlogn")
                  ^ "\n"
         fun formatReal a = StringCvt.padLeft #" " 20 (Real.fmt (StringCvt.FIX (SOME 8)) a)
         fun printLine (n,a,c) =
            let
              val ns =  StringCvt.padLeft #" " 20 (Int.toString n)
              val sa =  StringCvt.padLeft #" " 20 (Int.toString (Real.floor (real a * 1000.0/ c)))
              val sc = formatReal c
            in
              print (ns ^ sa ^ sc ^ "\n")
            end
         val C = Real.fmt (StringCvt.FIX (SOME 2)) av
       in
         (print title;
          map printLine L';
          print "------------------------------------------------------------\n";
          print ("                                 avarage" ^ (formatReal av) ^ "\n");
          print ("The estimated sort time function: T(n) = " ^ C ^ " n log(n)\n"))
       end
   fun normalEvalSortModified L = evalSortNModified (evalCompareNModified L) L

(* Q14.8 *)

fun checkTimeByDefault base n = 
  let 
    val array = genArray n
  in
    eval {prog=(ArrayQSort.sort Int.compare), input=array, size=Array.length, base=base}
  end;


fun compareWithDefault list = 
  let 
    val compareResults = map checkTimePerCompare list
    val constant = (foldr(fn ((a,b,c),R) => c+R ) 0.0 compareResults)/Real.fromInt(length list)
    fun base n = constant * nlogn n
    val results = map (checkTimeByDefault base) list
    val average = (foldr(fn ((a,b,c),R) => c+R ) 0.0 results)/Real.fromInt(length list)
  in
    print(padString("array size")^padString("time in cunit")^padString("T/(n log(n))")^"\n");
    map printLine results;
    print("---------------------------------------------------------------\n");
    print(padString(" ")^padString("average")^padString(Real.toString(average))^"\n");
    print("The estimated sort time function: T(n) = "^(Real.toString(real(Real.trunc(average*10.0))/10.0))^" n log (n)\n")
  end;
(* 
- compareWithDefault test_list;
          array size       time in cunit        T/(n log(n))
              500000                 276       2.84003058539
             1000000                 550       2.68776781843
             5000000                3202       2.80300324564
---------------------------------------------------------------
                                 average       2.77693388315
The estimated sort time function: T(n) = 2.7 n log (n)

The effects are derived from the modification of `checkTimePerCompare`.
- compareWithDefault test_list;
          array size       time in cunit        T/(n log(n))
              500000                 354          0.43019002
             1000000                 682          0.39360134
             5000000                2994          0.30952604
---------------------------------------------------------------
                                 average      0.377772465668
*)

   fun evalDefaultSort c L =
       let
         fun sort array =
               ArrayQSort.sort Int.compare array
         fun base n = c * nlogn n
         fun evalN n = 
           let val array = genArray n
           in eval {prog = sort, input = array, size = Array.length, base = base}
           end
         val L' = map evalN L
         val av = (foldr (fn ((_,_,x),y) => y+x) 0.0 L') / (Real.fromInt (List.length L'))
         val title = (StringCvt.padLeft #" " 20 "array size")
                     ^ (StringCvt.padLeft #" " 20 "time in cunit")
                  ^ (StringCvt.padLeft #" " 20 "C/nlogn")
                  ^ "\n"
         fun formatReal a = StringCvt.padLeft #" " 20 (Real.fmt (StringCvt.FIX (SOME 8)) a)
         fun printLine (n,a,c) =
            let
              val ns =  StringCvt.padLeft #" " 20 (Int.toString n)
              val sa =  StringCvt.padLeft #" " 20 (Int.toString (Real.floor (real a * 1000.0/ c)))
              val sc = formatReal c
            in
              print (ns ^ sa ^ sc ^ "\n")
            end
         val C = Real.fmt (StringCvt.FIX (SOME 2)) av
       in
         (print title;
          map printLine L';
          print "------------------------------------------------------------\n";
          print ("                                 avarage" ^ (formatReal av) ^ "\n");
          print ("The estimated sort time function: T(n) = " ^ C ^ " n log(n)\n"))
       end
   fun compareWithDefaultModified L = evalDefaultSort (evalCompareNModified L) L
(* 
- compareWithDefaultModified test_list;
          array size       time in cunit        T/(n log(n))
              500000                  42          0.05220039
             1000000                  83          0.04899116
             5000000                 395          0.04176475
---------------------------------------------------------------
                                 average          0.04765210
The estimated sort time function: T(n) = 0.05 n log (n)
val it = () : unit

- compareWithDefaultModified test_list;
          array size       time in cunit             C/nlogn
              500000               62474          3.02524997
             1000000              131548          3.04830879
             5000000              734365          3.55272734
------------------------------------------------------------
                                 avarage          3.20876204
The estimated sort time function: T(n) = 3.21 n log(n)
val it = () : unit

自作の方が、デフォルトで提供されているものより良いけれど、これはいいのだろうか？
*)
