fun length [] = 0(*calculates the length of a list*)
  | length (h::T) = 1+length(T);
fun ltu ([],num2) = not((num2=[])orelse(num2=[0]))
  | ltu ([0],num2) = not((num2=[])orelse(num2=[0]))
  | ltu (num1,[0]) = (num1=[])orelse(num1=[0])
  | ltu (num1,[]) = (num1=[])orelse(num1=[0])
  | ltu (h1::num1,h2::num2) = 
      if (length(h1::num1)<length(h2::num2)) then true
      else if (length(h1::num1)>length(h2::num2)) then false
      else
          if (h1<h2)then true
          else if (h1=h2) then ltu(num1,num2)
          else false;
fun lt(("+",x),("~",y)) = false
  | lt (("~",x),("+",y)) = true
  | lt(("+",x),("+",y)) = ltu(List.rev x,List.rev y)
  | lt(("~",x),("~",y)) = not (ltu(List.rev x,List.rev y));

fun rev([])=[]
  | rev(x::xs) = rev(xs)@[x];
fun plusu ([], ys) = ys
  | plusu (xs, []) = xs
  | plusu (xs, [0]) = xs
  | plusu ([0], ys) = ys
  | plusu (x::xs, y::ys) =
    let val z = x + y in z mod 10 :: plusu(xs, plusu(ys,[z div 10])) end;

fun minusu ([], ys) = List.map ~ op ys
  | minusu([0],ys)=List.map ~ ys
  | minusu(xs,[0])=xs
  | minusu (xs, []) = xs
  | minusu (x::xs, y::ys) =
    if x >= y then x - y :: minusu(xs, ys)
    else 10 + x - y :: minusu(minusu(xs,[1]), ys);

fun times ([], ys) = []
  | times (xs, []) = []
  | times (xs, [0]) = []
  | times ([0], ys) = []
  | times ([x], y::ys) = 
    if ((x*y div 10)>0) then x * y  mod 10 :: plusu(times([x], ys),[x*y div 10])
    else x * y  mod 10 :: times([x],ys)
  | times (x::xs, [y]) = times([y],x::xs)
  | times ((x::xs), ys) =
      plusu(times([x],ys),0::times(xs,ys));

fun addi(("+",x),("+",y)) = ("+",plusu(x,y))
  | addi(("~",x),("~",y)) = ("~",plusu(x,y))
  | addi(("+",x),("~",y)) = 
    if not(ltu(List.rev x,List.rev y)) then ("+",minusu(x,y))
    else ("~",minusu(y,x))
  | addi(("~",x),("+",y)) =
    if not(ltu(List.rev x,List.rev y)) then ("~",minusu(x,y))
    else ("+",minusu(y,x)) ;

fun subi(("+",x),("+",y)) = 
    if not(ltu(List.rev x,List.rev y)) then ("+",minusu(x,y))
    else ("~",minusu(y,x))
    (* ("+",minusu(x,y)) *)
  | subi(("~",x),("~",y)) = 
    if not(ltu(List.rev x,List.rev y)) then ("~",minusu(x,y))
    else ("+",minusu(y,x)) 
    (* ("~",minusu(x,y)) *)
  | subi(("+",x),("~",y)) = ("+",plusu(x,y))
  | subi(("~",x),("+",y)) = ("~",plusu(y,x));

fun muli(("+",x),("+",y)) = ("+",times(x,y))
  | muli(("~",x),("~",y)) = ("+",times(x,y))
  | muli(("+",x),("~",y)) = ("~",times(x,y))
  | muli(("~",x),("+",y)) = ("~",times(x,y));
fun fromIntu 0 = []
  | fromIntu n = 
      if(n>0) then (n mod 10) :: fromIntu (n div 10)
      else fromIntu (~n);
fun sign n = 
  if n>=0 then "+"
  else "~";
fun minimize((sign,x))=
  let
    fun removeZero([])=[]
    | removeZero(x::xs)=
        if  x<>0 then x::xs
        else removeZero xs
  in
    (sign , rev (removeZero(rev x)))
  end;
fun toStringu [] = ""
  | toStringu (x::xs) = Int.toString x ^ toStringu xs;

fun negative("+")="~"
  | negative("~")="+" ;
fun xor(s1,s2) =
  if (s1=s2) then "+"
  else "~";


fun eq([],[])=true
    | eq([],y)=false
    | eq(x,[])=false
    | eq(x::xs,y::ys)=(x=y) andalso eq(xs,ys)
 

fun sep(c) = (c= #".") orelse (c= #"(") orelse (c= #")");
fun Sep_int (x)=String.tokens(fn c => sep(c) )  x;
fun hasSign(x) = 
  let val s=String.substring (List.sub(Sep_int(x),0),0,1)
  in (s="+")orelse(s="~")
  end;
fun has_Rec(x) = (String.isSubstring "(" x )andalso(String.isSubstring ")" x) ;
fun has_nonRec(x)= not((String.isSubstring ".(" x ) andalso (String.isSubstring "." x));
fun f x = valOf(Int.fromString(str(x)))
fun fromString(x)=List.rev (List.map f (explode x));
fun Recpart(x) = 
  if has_Rec(x) andalso not (has_nonRec(x)) then 
    if hasSign(x) then (String.substring (List.sub(Sep_int(x),0),0,1),fromString(List.sub(Sep_int(x),1)))
    else ("+",fromString(List.sub(Sep_int(x),1)))
  else if has_Rec(x) andalso has_nonRec(x) then 
    if hasSign(x) then (String.substring (List.sub(Sep_int(x),0),0,1),fromString(List.sub(Sep_int(x),2)))
    else ("+",fromString(List.sub(Sep_int(x),2)))
  else ("+",[]);
fun nonRec(x)=
  if has_nonRec(x) then 
    if hasSign(x) then (String.substring (List.sub(Sep_int(x),0),0,1),fromString(List.sub(Sep_int(x),1)))
    else ("+",fromString(List.sub(Sep_int(x),1)))
  else ("+",[]);
fun IntPart(x) = 
  if hasSign(x) then (String.substring (List.sub(Sep_int(x),0),0,1),fromString(String.substring (List.sub(Sep_int(x),0),1,size (List.sub(Sep_int(x),0))-1)))
  else ("+",fromString(List.sub(Sep_int(x),0)));



fun power(0)=("+",[1])
  | power(n)=
    let val (s,p) = power(n-1)
    in (s,0::p)
    end;


fun minimize2(x)=
  let
    fun removeZero([])=[]
    | removeZero(x::xs)=
        if  x<>0 then x::xs
        else removeZero xs
  in
    rev (removeZero(rev x))
  end;
fun update_rem(rem,[]) = (rem,[])(* same as above except only for odd number of digits
    for odd no. of digits we have take only first digit and divide remaing digits into pairs*)
    | update_rem(rem,h::T) = (h::rem,T);
fun equal(x,y)=
    let val x = minimize2(x)
        val y=minimize2(y)
    in (x=y)
    end;
fun str_helper(x) = 
    let val x = explode x
    in List.rev(List.map f x )
    end;


fun reverse([])=[](*reverses the list*)
    | reverse (h::T) = reverse(T)@[h];
fun update3(quo,divi,rem,i) = 
    if (length rem)>0 then
        if ltu(List.rev (minimize2(rem)),List.rev divi) then (plusu(i,0::quo),divi,rem)
        else update3(quo,divi,minusu(rem,divi),plusu(i,[1]))
    else(quo,divi,rem);
fun divide_helper(divi,quo,[],num)=(quo,[])
  | divide_helper(divi,quo,rem,num)=
        if (num=[]) then 
            let 
                val (quo,divi,rem) = update3(quo,divi,rem,[0])
            in (minimize2(quo),minimize2(rem))
            end
        else
            let 
                val (quo,divi,rem) = update3(quo,divi,rem,[0])
                val (rem,num) = update_rem(rem,List.rev(num))
            in divide_helper(divi,quo,rem,List.rev num)
            end;


fun division(p,q)=
    if (q=[1]) then (p,[])
    else if (equal(p,q)) then ([1],[])
    else if (ltu(List.rev p,List.rev q)) then ([],p)
    else 
        let val n = length q
            val rem = List.take((List.rev p),n)
            val p = List.drop((List.rev p),n)
            in divide_helper(q,[],List.rev rem,List.rev p) 
        end;



fun gcd(x,y)=
  if (x=[0]) orelse (x=[]) then y
  else 
    let val (q,r)=division(y,x)
    in gcd(r,x)
    end;

fun minimalForm (p , q) =
  let
    val d = gcd (minimize2(p), minimize2(q))
    val (q1,r1)=division(p,d)
    val (q2,r2)=division(q,d)
  in
    (q1,q2)
  end;

fun negate(s)=
  if hasSign(s) then negative(str(String.sub(s,0)))^String.substring(s,1,(size s)-1)
  else "~"^s;
fun search([],key)=false
  | search(x::xs,key)=(x=key)orelse search(xs,key);
fun index(x,key,i)= 
  if not(search(x,key)) then NONE
  else 
      if(List.hd(x)=key) then SOME i else index(List.tl(x),key,i+1);

fun negative("+")="~"
  | negative("~")="+";

fun division2(p,q,quo)=
    if (ltu(List.rev p, List.rev q)) then (quo,p)
    else  division2(minimize2(minusu(p,q)),q,plusu(quo,[1]));
fun toStringu [] = ""
  | toStringu (x::xs) = Int.toString x ^ toStringu xs;
fun incr(dig,[])=0::dig
  | incr(dig,x)=hd(x)::dig;
fun frStr([]) = "0"
  | frStr(x) = toStringu(x);
fun update2(digits,rem,n,denom)=
    if not(search(rem,toStringu(List.rev n))) then 
      let val rem=(toStringu(List.rev n))::rem
          val (q,r)=division(n,denom)
          (* val (q,r)=division2(n,denom,[]) *)

          val digits = incr(digits,q)
          
          val n = 0::r
          (* val denom=denom *) 
      in update2(digits,rem,n,denom)
      end
    else (digits,rem,n);

fun toStr(digits)=
  if size(toStringu(List.rev digits)) = 1 then toStringu(List.rev digits)
  else String.substring(toStringu(List.rev digits),1,size(toStringu(List.rev digits)))
fun longdiv(num,denom)=
  let val digits=[]
      val rem=["0",""]
      val (n,r)=division(num,denom)
      val i=frStr(List.rev n)
      val (digits,rem,n)=update2(digits,rem,0::r,denom)
      in 
        if (n=[])orelse(n=[0]) then i^ "." ^ String.substring(toStringu(List.rev digits),0,size(toStringu(List.rev digits)))^"(0)"
        else 
          let val recurring = valOf(index(List.rev rem,toStringu(List.rev n),0))-2
          val result = toStringu(List.rev digits)
          in i ^ "." ^ String.substring(result,0,recurring) ^ "(" ^ String.substring(result,recurring,size(result)-recurring) ^ ")"
          end
  end;





signature BIGINT =
    sig
        type bigint = string*int list 
        
        val fromInt : int -> bigint
        val fromStr: string-> bigint
        val toString : bigint -> string
        val Add : bigint * bigint -> bigint
        val Sub : bigint * bigint -> bigint
        val Mul : bigint * bigint -> bigint
        val Div : bigint*bigint -> bigint
        val Mod : bigint*bigint -> bigint
        val Equal: bigint * bigint -> bool
        val LT : bigint * bigint -> bool
        val LE : bigint * bigint -> bool
        val GT : bigint * bigint -> bool
        val GE : bigint * bigint -> bool
        val NE : bigint * bigint -> bool
        
        
    end

structure BigInt : BIGINT =
struct

    type bigint=string*int list 
    fun fromInt n =(sign n, fromIntu n);
    fun fromStr(x) = 
      if ((String.sub(x,0))= #"+") orelse(((String.sub(x,0))= #"~"))
        then (str(String.sub(x,0)),str_helper(String.substring(x,1,String.size(x)-1)))
      else ("+",str_helper(x));
    fun toString ((sign,[])) = "0"
      | toString ((sign,xs)) =
        if (sign = "~") then sign^(toStringu (List.rev xs))
        else toStringu (List.rev xs);
        
    fun Add(x,y)=minimize(addi(x,y));
    fun Sub(x,y)=minimize(subi(x,y));
    fun Mul(x,y)=minimize(muli(x,y));
    fun Div(x,y) = 
      let val (s1,num1) = x
          val (s2,num2) = y
          val (n,r) = division(num1,num2)
      in (xor(s1,s2),n)
      end;
    fun Mod(x,y) = 
      let val (s1,num1) = x
          val (s2,num2) = y
          val (n,r) = division(num1,num2)
      in (xor(s1,s2),r)
      end;
    fun Equal(a,b) = 
      let val (sign1,x)=a
          val (sign2,y)=b
      in (sign1 = sign2) andalso (x=y)
      end;
    fun LT(x,y) = lt(x,y);
    fun LE(x,y) = lt(x,y) orelse Equal(x,y);
    fun GT(x,y) = not(LE(x,y));
    fun GE(x,y) = not(LT(x,y));
    fun NE(x,y) = not(Equal(x,y))

      
      
end;

signature RATIONAL =
    sig
        type rational=string *int list*int list
        exception rat_error
        include BIGINT
        val make_rat: bigint * bigint -> rational option
        val rat: bigint -> rational option
        val reci: bigint -> rational option
        val neg: rational -> rational
        val inverse : rational -> rational option
        val equal : rational * rational -> bool (* equality *)
        val less : rational * rational -> bool (* less than *)
        val LER : rational*rational ->bool
        val GTR : rational*rational ->bool
        val GER : rational * rational -> bool
        val NER : rational * rational -> bool
        val add : rational * rational -> rational (* addition *)
        val subtract : rational * rational -> rational (* subtraction *)
        val multiply : rational * rational -> rational (* multiplication *)
        val divide : rational * rational -> rational option (* division *)
        val showRat : rational -> string
        val showDecimal : rational -> string
        val fromDecimal : string -> rational
        val toDecimal : rational -> string
    end;
  
functor Rational(BigInt:BIGINT) =
struct
  exception rat_error
  type rational=string *int list*int list;
  type bigint = string*int list;
  fun make_rat(x:bigint,y:bigint)=
    let val (s1,num) = x
        val (s2,denom)=y
    in
      if not (denom=[0] orelse denom=[]) then 
        let val (num,denom) = minimalForm(num,denom)
            val res:rational=(xor(s1,s2),num,denom)
        in SOME res
        end
      else 
        raise rat_error
    end;
    
  fun rat(x:bigint)=
    let val (sign,num) = x
      in make_rat((sign,num),("+",[1])) 
    end;
  
  fun reci(x:bigint) = 
    let val (sign,num) = x
    in make_rat(("+",[1]),x)
    end;
  fun neg(x:rational) = 
    let val (sign,num,denom)=x
        val res:rational = (negative(sign),num,denom)
    in
      res
    end;
  
  fun inverse(x:rational) = 
    let
      val (sign,num,denom)=x
    in
      make_rat(("+",denom),(sign,num))
    end;
  (* fun equal({fraction=(s1,n1,d1),decimal=k1},{fraction=(s2,n2,d2),decimal=k2}) = (k1=k2); *)
  fun equal(x:rational,y:rational) =
    let
      val (s1,n1,d1)=x
      val (s2,n2,d2)=y
    in
      (s1=s2) andalso (n1=n2) andalso (d1=d2)
    end;
  fun less(x:rational,y:rational) = 
    let
      val (s1,n1,d1)=x
      val (s2,n2,d2)=y
    in
      lt(BigInt.Mul((s1,n1),("+",d2)),BigInt.Mul(("+",d1),(s2,n2)))
    end;
  fun LER(x:rational,y:rational) = less(x,y) orelse equal(x,y);
  fun GTR(x:rational,y:rational) = not (LER(x,y));
  fun GER(x:rational,y:rational) = not(less(x,y));
  fun NER(x,y) = not(equal(x,y))
  fun add(x:rational,y:rational) = 
    let
      val (s1,n1,d1)=x
      val (s2,n2,d2)=y
    in
      let val num = BigInt.Add(BigInt.Mul((s1,n1),("+",d2)),BigInt.Mul(("+",d1),(s2,n2)))
          val denom = BigInt.Mul(("+",d1),("+",d2))
      in valOf(make_rat(num,denom))
      end
    end;
  fun subtract(x:rational,y:rational) = 
    let
      val (s1,n1,d1)=x
      val (s2,n2,d2)=y
    in
      let val num = BigInt.Sub(BigInt.Mul((s1,n1),("+",d2)),BigInt.Mul(("+",d1),(s2,n2)))
          val denom = BigInt.Mul(("+",d1),("+",d2))
      in valOf(make_rat(num,denom))
      end
    end;
  fun multiply(x:rational,y:rational) = 
    let
      val (s1,n1,d1)=x
      val (s2,n2,d2)=y
    in
      let val num =BigInt.Mul((s1,n1),(s2,n2))
          val denom =BigInt.Mul(("+",d1),("+",d2))    
      in  valOf(make_rat(num,denom))
      end
    end;
  fun divide(x:rational,y:rational) = 
    let
      val (s1,n1,d1)=x
      val (s2,n2,d2)=y
    in
      let val num =BigInt.Mul((s1,n1),("+",d2))
          val denom =BigInt.Mul(("+",d1),(s2,n2))   
      in  make_rat(num,denom)
      end
    end;
  
  fun showRat(x:rational)=
    let
      val (sign,num,denom)=x 
    in sign^toStringu(List.rev num)^"/"^toStringu(List.rev denom)
    end;
  fun showDecimal(x:rational)=
    let
      val (sign,num,denom)=x 
    in sign^longdiv(num,denom)
    end;
  fun fromDecimal(num:string)=
    if (size num)=0 then raise rat_error
    else
      let val intpart = valOf(make_rat(IntPart(num),("+",[1])))
          val (s,nonrec)=nonRec(num)
          val (s1,reci)=Recpart(num)
          val nonrecpart  = valOf(make_rat(nonRec(num),power(length nonrec)))
          val base=BigInt.Sub(power((length nonrec)+(length reci)),power((length nonrec)))
          val recpart = valOf(make_rat(Recpart(num),base))
          
      in add(add(intpart,nonrecpart),recpart)
      end;
  fun toDecimal(x:rational)=
    let
      val (sign,num,denom)=x 
    in sign^longdiv(num,denom)
    end;

end;

