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
