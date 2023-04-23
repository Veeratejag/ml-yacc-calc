
fun remove_n([]) = []
    | remove_n(h::T) = (String.substring(h,0,String.size(h)-1))::remove_n(T);
fun paragraph s  = "<p>"^s^"</p>";   
fun header(s)=
    if  String.isPrefix "######" s   then "<h6>"^String.substring(s,6,String.size(s)-6)^"</h6>"
    else if  String.isPrefix  "#####" s   then "<h5>"^String.substring(s,5,String.size(s)-5)^"</h5>" 
    else if  String.isPrefix "####" s  then "<h4>"^String.substring(s,4,String.size(s)-4)^"</h4>" 
    else if  String.isPrefix "###" s  then "<h3>"^String.substring(s,3,String.size(s)-3)^"</h3>" 
    else if  String.isPrefix "##" s  then "<h2>"^String.substring(s,2,String.size(s)-2)^"</h2>" 
    else if String.isPrefix "#" s then  "<h1>"^String.substring(s,1,String.size(s)-1)^"</h1>" 
    else s;


fun linebreak([])=true
    | linebreak(h::[]) =true
    | linebreak(h::T) = (List.hd(T)="");

fun check_headers([],parastart,paraend)=[]
    | check_headers (h::T,parastart,paraend) = 
        if String.isPrefix "#" h then (header(h))::check_headers(T,parastart,paraend)
        (* else if String.isPrefix "======" List.hd(T) then ("<h1>"^h^"</h1>")::check_headers(List.tl(T))
        else if  String.isPrefix "------" List.hd(T) then ("<h2>"^h^"</h2>")::check_headers(List.tl(T)) *)
        else 
            if linebreak(h::T) andalso (parastart =0) andalso(paraend=0) andalso (not (h=""))  then (paragraph h)::check_headers(T,0,0) 
            else if (parastart = 0) andalso not(linebreak (h::T)) then ("<p>"^h)::check_headers(T,1,0)
            else if (parastart=1)andalso linebreak(h::T) then (h^"</p>")::check_headers(T,0,0)
            else h::check_headers(T,parastart,paraend);
fun add_n([])=[]
    | add_n(h::T) = (h^"\n")::add_n(T);



fun horizontal s =
    if String.isPrefix "---" s then "<hr>"
    else s;
fun check_hr [] = []
    | check_hr (h::T) = (horizontal h)::check_hr(T);



fun replaceSubstring (str : string, oldSub : string, newSub : string) =
  let
    fun recurse (i : int) =
      if i + String.size oldSub > String.size str then
        String.substring (str, i, String.size str - i)
      else if String.substring (str, i, String.size oldSub) = oldSub then
        newSub ^ recurse (i + String.size oldSub)
      else
        String.substring (str, i, 1) ^ recurse (i + 1)
  in
    recurse (0)
  end;

fun code_check_start (s,pre_lt,pre_and) = 
    if (String.isSubstring "`&" s )  then
        replaceSubstring(s,"`<","<code>&amp;")
    else if (String.isSubstring "`<" s ) then 
        replaceSubstring(s,"`<","<code>&lt;")
    else if (String.isPrefix "   `<" s) then 
        let val pre_lt=1 
        in replaceSubstring(s,"   `<","<pre><code>&lt;")
        end
    else if (String.isPrefix "   `&" s ) then 
        let val pre_and=1
        in replaceSubstring(s,"   `&","<pre><code>&amp;")
        end
    else s;

fun code_check_end (s,pre_lt,pre_and) = 
    if (String.isSubstring "&`" s ) then
        if (pre_and=1) then
        replaceSubstring(s,"&`",";</code></pre>")
        else 
        replaceSubstring(s,"&`",";</code>")
    else if (String.isSubstring ">`" s ) then
        if (pre_lt =1) then
        replaceSubstring(s,">`","&gt;</code></pre>")
        else 
        replaceSubstring(s,">`","&gt;</code>")
    
    else s;

fun blockquote s = 
    if String.isPrefix ">" s then  "<blockquote> <p>"^String.substring(s,1,String.size(s)-1)^"</p> </blockquote>" 
    else s; 
fun block_check []=[]
    | block_check (h::T) = (blockquote h)::block_check(T);
fun table ([],start)=[]
    | table (h::T,start) = 
        if (start=0) andalso (h="<<") then 
            ("<CENTER><TABLE border="^"1"^">")::table(T,1)
        else if (start=1) andalso (h=">>") then 
            ("</TABLE></CENTER>")::table(T,0)
        else if (start=1) then 
            ("<TR><TD>"^replaceSubstring(h,"|","</TD><TD>")^"</TD></TR>")::table(T,start)
        else h::table(T,start);

fun code_check [] = []
    | code_check (h::T) = (code_check_end( code_check_start(h,0,0),0,0)) :: code_check(T); 



fun emphasis ([],italic,bold,underline) = "" 
    | emphasis (h::T,italic,bold,underline) = 

        if (h= #"*") andalso (bold=0) andalso (not(T=[])) andalso(List.hd(T)= #"*") then 
            emphasis(T,italic,1,underline)
        else if (h= #"*") andalso  (not(T=[]) andalso bold=1) then
            "<strong>"^emphasis(T,italic,2,underline)
        else if (h= #"*") andalso ((not(T=[])) andalso List.hd(T)= #"*") andalso (bold=2) then
            emphasis(T,italic,3,underline)   
        else if (h= #"*") andalso  (bold=3) andalso (T=[] orelse List.hd(T)= #" ") then
            "</strong>"^emphasis(T,italic,0,underline)
        else if (h= #"*") andalso ((not(T=[])) andalso italic=0) andalso not(List.hd(T)= #"*") then
            "<em>"^emphasis(T,1,bold,underline)
        else if (h= #"*") andalso (italic=1) andalso (T=[] orelse List.hd(T)= #" ") then
            "</em>"^emphasis(T,0,bold,underline)
        else if (h= #"_") andalso ((not(T=[])) andalso underline=0) andalso (List.hd(T)= #"_") then 
            emphasis(T,italic,bold,1)
        else if (h= #"_") andalso ((not(T=[])) andalso List.hd(T)= #"_") andalso (underline=1) then
            "<u>"^emphasis(T,italic,bold,2)
        else if (h= #"_") andalso ((not(T=[])) andalso List.hd(T)= #"_") andalso (underline=2) then
            emphasis(T,italic,bold,3)   
        else if (h= #"_") andalso (T=[] orelse List.hd(T)= #" ") andalso (underline=3) then
            "</u>"^emphasis(T,italic,bold,0)
        else 
            Char.toString(h)^emphasis(T,italic,bold,underline);

fun em_check(s) = 
    let val s = explode s
    in emphasis(s,0,0,0)
    end;

fun em_total([])=[]
    | em_total (h::T) = em_check(h)::em_total(T);
fun outdata(outfile,[]) = outfile
    | outdata(outfile,h::T) = 
        let val x = TextIO.output(outfile, h)
        in outdata(outfile,T)
        end;
fun mdt2html filename = 
    let 
        
        val in_channel = TextIO.openIn (filename^".mdt")
        fun readLines lines =
                case TextIO.inputLine in_channel of
                    NONE => lines
                | SOME line => readLines (line :: lines)
        val outFile=TextIO.openOut (filename ^ ".html")
        val outfile = TextIO.output(outFile, "<html>\n")
        val outfile = TextIO.output(outFile, "<body>\n")
        val lines =  readLines []
        val lines2 = List.rev(lines)                    
        val lines3 = remove_n(lines2)
                       
        val lines4 = code_check(check_hr(check_headers(lines3,0,0)))
        val lines4 = table(lines4,0)
        val lines5 = add_n(block_check(em_total lines4))
        val outFile = outdata(outFile,lines5)
        val outfile = TextIO.output(outFile, "</body>\n")
        val outfile = TextIO.output(outFile, "</html>\n")
        (* val linebreak = String.translate (fn #"\n" => "<br>") header *)

    in
        TextIO.closeOut (outFile)
            
    end;  

