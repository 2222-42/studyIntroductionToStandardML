signature FILEUTIL =
sig
  val exists : string -> string -> bool
  val touchDir : {isAbs:bool, vol:string, arcs: string list} -> unit
end

structure FileUtil : FILEUTIL =
struct
  (* Q18.30 *)
  fun exists fileName dirName =
    let 
      val d = OS.FileSys.openDir dirName
      fun copyDirStream d list =
          case OS.FileSys.readDir d of
            NONE => list before OS.FileSys.closeDir d
          | SOME item =>
            copyDirStream d (list @ [item])
    in
      List.exists (fn x => x = fileName) (copyDirStream d [])
    end
  fun touchDir {isAbs, vol, arcs} = 
    let
      val pathString = OS.Path.toString {arcs=arcs, isAbs=isAbs, vol=vol}
      val canonicPathString = OS.Path.mkCanonical(pathString)
      val targetPath = OS.Path.fromString canonicPathString
    in
      print (OS.Path.toString targetPath)
      (* TODO: 冗長性が取り除かれているか確認する *)
      (* TODO: exists 関数で各段階であるかどうかをテストする *)
      (* TODO: なければ作成する *)
    end
    
end

(* 
FileUtil.exists "test.txt" "/mnt/e/SMLProject/studyIntroductionToStandardML/";

*)