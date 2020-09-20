(* SML source file. Copyright (c) by 2222-42 2020.
* Chap17.1 Q17.1
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
        case F.readDir d of
           NONE => F.closeDir d
         | SOME v => (Format.printf defaultFormat2 
                                    [Format.S (getModeStr v), 
                                     Format.I (getSize v), 
                                     Format.S (getTime v),
                                     Format.S v];
                      printRest())
      end
  in
    Format.printf defaultFormat [Format.S "dlrwx", Format.S "file size", Format.S "last modified", Format.S "file name"];
    printRest()
  end

(* 筆者の回答 *)
   fun lsByAuthor () =
       let
         val d = F.openDir (F.getDir())
         fun printRest () =
             case F.readDir d of
               NONE => F.closeDir d
             | SOME f =>
               let
              val size = Int64.toInt (F.fileSize f)
          handle _ => 0
              val time = Date.toString (Date.fromTimeLocal (F.modTime f))
                 val modString = implode
                                   [if F.isDir f then #"d" else #"-",
                                    if F.isLink f then #"l" else #"-",
                                    if F.access (f, [F.A_READ]) then #"r" else #"-",
                                    if F.access (f, [F.A_WRITE]) then #"w" else #"-",
                                    if F.access (f, [F.A_EXEC]) then #"x" else #"-"]
            in
                 (Format.printf
                    "%10s%15d%30s%30s\n"
                    [Format.S modString,
                     Format.I size,
                     Format.S time,
                     Format.S f
                    ];
               printRest())
            end
       in
         (Format.printf
            "%10s%15s%30s%30s\n"
            [Format.S "dlrwx",
             Format.S "file size",
             Format.S "last modified",
             Format.S "file name"
            ];
          printRest()
         )
       end

(* signature OS_PATH =     
  sig
    exception Path      
    exception InvalidArc
    val parentArc : string
    val currentArc : string
    val fromString : string -> {arcs:string list, isAbs:bool, vol:string}
    val toString : {arcs:string list, isAbs:bool, vol:string} -> string
    val validVolume : {isAbs:bool, vol:string} -> bool
    val getVolume : string -> string
    val getParent : string -> string
    val splitDirFile : string -> {dir:string, file:string}
    val joinDirFile : {dir:string, file:string} -> string
    val dir : string -> string
    val file : string -> string
    val splitBaseExt : string -> {base:string, ext:string option}
    val joinBaseExt : {base:string, ext:string option} -> string
    val base : string -> string
    val ext : string -> string option
    val mkCanonical : string -> string
    val isCanonical : string -> bool
    val mkAbsolute : {path:string, relativeTo:string} -> string
    val mkRelative : {path:string, relativeTo:string} -> string
    val isAbsolute : string -> bool
    val isRelative : string -> bool
    val isRoot : string -> bool
    val concat : string * string -> string
    val fromUnixPath : string -> string
    val toUnixPath : string -> string
  end *)

structure P = OS.Path;
P.isAbsolute "../chapter17/test.txt";
P.isAbsolute fullPath;
P.isRelative "../chapter17/test.txt";
P.isRelative "../chapter17/test.txt";
P.validVolume {isAbs=true, vol=fullPath};
val pathStrRecords = P.fromString fullPath;
P.toString pathStrRecords;
P.getVolume fullPath;
P.getParent fullPath;
val splitExt = P.splitBaseExt fullPath;
P.isCanonical(#base splitExt);
P.mkCanonical(fullPath);

val currentDir = F.getDir();
val currentPath = P.fromString(currentDir);
val currentPathStr = P.toString(currentPath);
val parentPath = P.getParent(currentPathStr);
val splitedDirFile = P.splitDirFile parentPath;
val joinedDirFile = P.joinDirFile splitedDirFile;
val path = P.concat(joinedDirFile, "chapter_17_sample");
P.mkRelative{path=path, relativeTo=P.getParent(nowDirStr)};
