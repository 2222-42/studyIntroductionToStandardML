(* SML source file. Copyright (c) by 2222-42 2020.
* Chap17.1
*)

(* - signature X = OS_FILE_SYS; 
[autoloading]
[library $SMLNJ-BASIS/basis.cm is stable]
[library $SMLNJ-BASIS/(basis.cm):basis-common.cm is stable]
[autoloading done]
signature OS_FILE_SYS =
  sig
    type dirstream
    val openDir : string -> dirstream
    val readDir : dirstream -> string option
    val rewindDir : dirstream -> unit
    val closeDir : dirstream -> unit
    val chDir : string -> unit
    val getDir : unit -> string
    val mkDir : string -> unit
    val rmDir : string -> unit
    val isDir : string -> bool
    val isLink : string -> bool
    val readLink : string -> string
    val fullPath : string -> string
    val realPath : string -> string
    val fileSize : string -> Int64.int
    val modTime : string -> Time.time
    val setTime : string * Time.time option -> unit
    val remove : string -> unit
    val rename : {new:string, old:string} -> unit
    datatype access_mode = A_EXEC | A_READ | A_WRITE
    val access : string * access_mode list -> bool
    val tmpName : unit -> string
    eqtype file_id
    val fileId : string -> file_id
    val hash : file_id -> word
    val compare : file_id * file_id -> order
  end *)

structure F = OS.FileSys;
val nowDirStr = F.getDir();
val newDir = F.openDir(nowDirStr^"/chapter17");
F.readDir newDir;
F.chDir (nowDirStr^"/chapter17");
F.getDir();
val filePath = F.realPath "../chapter17/test.txt";
val fullPath = F.fullPath "../chapter17/test.txt";
(* F.realPath "../chapter17/test2.txt";
F.fullPath "../chapter17/test2.txt"; *)

(* Q17.1 *)
use "../chapter16/chap16_3_q9.sml";
fun ls () =
  let
    val d = F.openDir (F.getDir())
    val defaultFormat = "%5s%20s%20s%20s\n"
    val defaultFormat2 = "%5s%20d%20s%20s\n"
    fun printRest() = 
      let
        val f = F.readDir d
        fun getAccessModeStr fileName = 
          (if F.access (fileName, [F.A_READ]) then "r" else "-") ^
          (if F.access (fileName, [F.A_WRITE]) then "w" else "-") ^
          (if F.access (fileName, [F.A_EXEC]) then "x" else "-")
        fun getModeStr fileName = 
          (if F.isDir fileName then "d" else "-")^
          (if F.isLink fileName then "l" else "-") ^
          (getAccessModeStr fileName)
        fun getSize fileName =
          Int64.toInt (F.fileSize fileName)
          handle _ => 0
        fun getTime fileName = 
            Date.fmt "%b %d %H:%M:%S" (Date.fromTimeLocal(F.modTime fileName))
            handle _ => ""
      in
        case f of
           NONE => ()
         | SOME v => Format.printf defaultFormat2 
                                    [Format.S (getModeStr v), 
                                     Format.I (getSize v), 
                                     Format.S (getTime v),
                                     Format.S v];
                      printRest()
      end
  in
    Format.printf defaultFormat [Format.S "dlrwx", Format.S "file size", Format.S "last modified", Format.S "file name"];
    printRest()
  end
