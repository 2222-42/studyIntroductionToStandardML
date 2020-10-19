signature COPYURL =
sig
  val copy : Types.url -> Types.url -> unit
end

structure CopyUrl : COPYURL =
struct
  local
    open Url
  in
    fun printMessage fromUrl toUrl = ()
    fun guessHtml url = true
    fun copyUrl fromUrl = true
    fun copy fromUrl toUrlDir =
      let
        fun splitDirFile L = 
          case L of
            nil => (nil, "")
          | [h] => (nil, h)
          | _ => (List.take(L, List.length L - 1), List.last L)
        val fromUrl' = canonicalUrl fromUrl
        val toUrlDir = canonicalUrl toUrlDir
        val fromBase = baseUrl fromUrl'
        val (dirPath, file) = splitDirFile (pathUrl fromUrl')
        val toDir = joinUrlPath toUrlDir dirPath
        val toUrl = joinUrlFile toDir file
        val toPath = case toDir of
           FILE{path,...} => OS.Path.toString {arcs=path, isAbs=true, vol=""}
         | _ => raise Runtime "The target must be a file"
      in
        if FileUtil.exists file toPath then ()
        else
          (printMessage(fromUrl, toUrl);
           FileUtil.touchDir {isAbs=true, arcs=(pathUrl toDir), vol=""};
           CopyUrl fromUrl' toUrl;
           if guessHtml toUrl then
             let
               val (PAGE{links,...}) = ParseHtml.parseHtml fromBase toUrl
               fun copyAll nil = ()
                 | copyAll (h::tail) = 
                      if Url.isRelative h then
                        (copy h toUrlDir; copyAll tail)
                      else copyAll tail
             in
               copyAll links
             end
            else ()
          )
      end
  end

end