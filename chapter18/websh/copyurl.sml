signature COPYURL =
sig
  val copy : Types.url -> Types.url -> unit
end

structure CopyUrl : COPYURL =
struct
  local
    open Url Types Control
  in
    fun printMessage (fromUrl, toUrl) = 
      print ("copy\n    source: "^(urlToString fromUrl)^"\n    target: "^(urlToString toUrl))
    fun guessHtml url =
        case (nodeUrl url) of
            SOME v => 
            ((String.isSuffix ".html" v) orelse (String.isSuffix ".htm" v) orelse
            (String.isSuffix ".shtml" v) orelse (String.isSuffix ".shtm" v))
          | NONE => false
    fun copyUrl fromUrl toUrl = 
      let
        val ins = ExternalIO.openIn fromUrl
        val outs = ExternalIO.openOut toUrl
      in
        ExternalIO.copyStream ins outs
      end
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
           copyUrl fromUrl' toUrl;
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