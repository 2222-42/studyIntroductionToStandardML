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
  (* Q18.31 *)
  fun touchDir {isAbs, vol, arcs} = 
    let
      val pathString = OS.Path.toString {arcs=arcs, isAbs=isAbs, vol=vol}
      val canonicPathString = OS.Path.mkCanonical(pathString)
      val targetPath = OS.Path.fromString canonicPathString
    in
      print (OS.Path.toString targetPath);
      (* 冗長性が取り除かれているか確認する -> DONE *)
      foldl (fn (x, R) => (print R; 
                          (* exists 関数で各段階でDirectoryあるかどうかをテストする *)
                           if exists x R then
                            ()
                           else
                            (* なければ作成する *)
                            OS.FileSys.mkDir (R^"/"^x);
                           R^"/"^x) ) "/" ((fn {arcs,...} => arcs)targetPath);
      ()
    end
    
end

(* 
FileUtil.exists "mnt" "/";

FileUtil.exists "test.txt" "/mnt/e/SMLProject/studyIntroductionToStandardML/";

FileUtil.touchDir {isAbs=true, vol="", arcs=["mnt", "e", "SMLProject", "studyIntroductionToStandardML", "testDir", "testSubDir", "..", "testSubDir1"]};
FileUtil.touchDir {isAbs=true, vol="", arcs=["mnt", "e", "SMLProject", "studyIntroductionToStandardML", "testDir", "testSubDir", "..", "testSubDir2"]};

*)