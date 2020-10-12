signature PARSEHTML =
sig
  val parseHtml : Types.url -> Types.url -> Types.value
end

structure ParseHtml =
struct
local open Types
in
  fun parseHtml url1 url2 =
    PAGE{url=url2, links=nil}
end
end
