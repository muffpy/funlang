// A parser for the Fun language
//================================
//
// call with 
//
//     amm fun_parser.sc fact.fun
//
//     amm fun_parser.sc defs.fun
//
// this will generate a parse-tree from a list
// of tokens

import scala.language.implicitConversions    
import scala.language.reflectiveCalls

import $file.fun_tokens, fun_tokens._ 


// Parser combinators
//    type parameter I needs to be of Seq-type
//
abstract class Parser[I, T](implicit ev: I => Seq[_]) {
  def parse(ts: I): Set[(T, I)]

  def parse_single(ts: I) : T = 
    parse(ts).partition(_._2.isEmpty) match {
      case (good, _) if !good.isEmpty => good.head._1
      case (_, err) => { 
	println (s"Parse Error\n${err.minBy(_._2.length)}") ; sys.exit(-1) }
    }
}

// convenience for writing grammar rules
case class ~[+A, +B](_1: A, _2: B)

class SeqParser[I, T, S](p: => Parser[I, T], 
                         q: => Parser[I, S])(implicit ev: I => Seq[_]) extends Parser[I, ~[T, S]] {
  def parse(sb: I) = 
    for ((head1, tail1) <- p.parse(sb); 
         (head2, tail2) <- q.parse(tail1)) yield (new ~(head1, head2), tail2)
}

class AltParser[I, T](p: => Parser[I, T], 
                      q: => Parser[I, T])(implicit ev: I => Seq[_]) extends Parser[I, T] {
  def parse(sb: I) = p.parse(sb) ++ q.parse(sb)   
}

class FunParser[I, T, S](p: => Parser[I, T], 
                         f: T => S)(implicit ev: I => Seq[_]) extends Parser[I, S] {
  def parse(sb: I) = 
    for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
}

// convenient combinators
implicit def ParserOps[I, T](p: Parser[I, T])(implicit ev: I => Seq[_]) = new {
  def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}

/* 2 different list parsers. The first accounts for function calls whereas 
* the second accounts for function definitions
*/
def ListParser[I, T, S](p: => Parser[I, T], 
                        q: => Parser[I, S])(implicit ev: I => Seq[_]): Parser[I, List[T]] = {
  (p ~ q ~ ListParser(p, q)) ==> { case x ~ _ ~ z => x :: z : List[T] } ||
  (p ==> ((s) => List(s)))
}
def ListParser2[I, A, T, S](p: => Parser[I, A], t: => Parser[I,T], 
                        q: => Parser[I, S])(implicit ev: I => Seq[_]): Parser[I, List[(A,T)]] = {
  (p ~ t ~ q ~ ListParser2(p, t, q)) ==> { case x ~ y ~ _ ~ z => (x,y) :: z : List[(A,T)] } ||
  (p ~ t) ==> { case x ~ y => List((x,y)) } // base case of only 1 arg
}

case class TokParser(tok: Token) extends Parser[List[Token], Token] {
  def parse(ts: List[Token]) = ts match {
    case t::ts if (t == tok) => Set((t, ts)) 
    case _ => Set ()
  }
}

implicit def token2tparser(t: Token) = TokParser(t)

implicit def TokOps(t: Token) = new {
  def || (q : => Parser[List[Token], Token]) = new AltParser[List[Token], Token](t, q)
  def ==>[S] (f: => Token => S) = new FunParser[List[Token], Token, S](t, f)
  def ~[S](q : => Parser[List[Token], S]) = new SeqParser[List[Token], Token, S](t, q)
}

case object NumParser extends Parser[List[Token], Int] {
  def parse(ts: List[Token]) = ts match {
    case T_INT(n)::ts => Set((n, ts)) 
    case _ => Set ()
  }
}
case object FloatParser extends Parser[List[Token], Double] {
  def parse(ts: List[Token]) = ts match {
    case T_FL(n)::ts => Set((n, ts)) 
    case _ => Set ()
  }
}
case object CharParser extends Parser[List[Token], Int] {
  def parse(ts: List[Token]) = ts match {
    case T_CHAR(s)::ts => Set((s, ts)) 
    case _ => Set ()
  }
}

case object IdParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_ID(s)::ts => Set((s, ts)) 
    case _ => Set ()
  }
}
case object GlobalIdParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_GLOBALID(s)::ts => Set((s, ts)) 
    case _ => Set ()
  }
}

case object TypeParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_TY(s)::ts => Set((s, ts)) 
    case _ => Set ()
  }
}



// Abstract syntax trees for the Fun language
abstract class Exp extends Serializable 
abstract class BExp extends Serializable 
abstract class Decl extends Serializable 

case class Def(name: String , args: List[(String , String)], 
 ty: String , body: Exp) extends Decl 
case class Main(e: Exp) extends Decl
case class Const(name: String , v: Int) extends Decl
case class FConst(name: String , x: Double) extends Decl 

case class Call(name: String , args: List[Exp]) extends Exp
case class If(a: BExp, e1: Exp, e2: Exp) extends Exp
case class Var(s: String) extends Exp
case class GlobalVar(s: String) extends Exp
case class Num(i: Int) extends Exp // integer numbers
case class FNum(i: Double) extends Exp // floating numbers
case class ChConst(c: Int) extends Exp // char constants
case class Aop(o: String , a1: Exp, a2: Exp) extends Exp
case class Sequence(e1: Exp, e2: Exp) extends Exp
case class Bop(o: String , a1: Exp, a2: Exp) extends BExp



// Grammar Rules for the Fun language


// arithmetic expressions
lazy val Exp: Parser[List[Token], Exp] = 
  (T_KWD("if") ~ BExp ~ T_KWD("then") ~ Exp ~ T_KWD("else") ~ Exp) ==>
    { case _ ~ x ~ _ ~ y ~ _ ~ z => If(x, y, z): Exp } ||
  (M ~ T_SEMI ~ Exp) ==> { case x ~ _ ~ y => Sequence(x, y): Exp } || 
  (T_LCURLY ~ Exp ~ T_RCURLY) ==> { case _ ~ y ~ _ => y: Exp } || 
  (T_LPAREN ~ Exp ~ T_RPAREN) ==> { case _ ~ y ~ _ => y: Exp } || M
lazy val M: Parser[List[Token], Exp] = L
lazy val L: Parser[List[Token], Exp] =
  (T ~ T_OP("+") ~ Exp) ==> { case x ~ _ ~ z => Aop("+", x, z): Exp } ||
  (T ~ T_OP("-") ~ Exp) ==> { case x ~ _ ~ z => Aop("-", x, z): Exp } || T  
lazy val T: Parser[List[Token], Exp] = 
  (F ~ T_OP("*") ~ T) ==> { case x ~ _ ~ z => Aop("*", x, z): Exp } || 
  (F ~ T_OP("/") ~ T) ==> { case x ~ _ ~ z => Aop("/", x, z): Exp } || 
  (F ~ T_OP("%") ~ T) ==> { case x ~ _ ~ z => Aop("%", x, z): Exp } || F
lazy val F: Parser[List[Token], Exp] = 
  (IdParser ~ T_LPAREN ~ ListParser(Exp, T_COMMA) ~ T_RPAREN) ==>  // function calls with args
    { case x ~ _ ~ z ~ _ => Call(x, z): Exp } ||
  (IdParser ~ T_LPAREN ~ T_RPAREN) ==>  // function calls with no args
    { case x ~ _ ~ _ => Call(x, List()): Exp } ||
  IdParser ==> { case x => Var(x): Exp } ||
  NumParser ==> { case x => Num(x): Exp } ||
  FloatParser ==> { case x => FNum(x): Exp } ||
  CharParser ==> { case x => ChConst(x): Exp } ||
  GlobalIdParser ==> { case x => GlobalVar(x) : Exp}

// boolean expressions
lazy val BExp: Parser[List[Token], BExp] = 
  (Exp ~ T_OP("==") ~ Exp) ==> { case x ~ _ ~ z => Bop("==", x, z): BExp } || 
  (Exp ~ T_OP("!=") ~ Exp) ==> { case x ~ _ ~ z => Bop("!=", x, z): BExp } || 
  (Exp ~ T_OP("<") ~ Exp)  ==> { case x ~ _ ~ z => Bop("<",  x, z): BExp } || 
  (Exp ~ T_OP(">") ~ Exp)  ==> { case x ~ _ ~ z => Bop("<",  z, x): BExp } || 
  (Exp ~ T_OP("<=") ~ Exp) ==> { case x ~ _ ~ z => Bop("<=", x, z): BExp } || 
  (Exp ~ T_OP("=>") ~ Exp) ==> { case x ~ _ ~ z => Bop("<=", z, x): BExp }  

// functions and globals
lazy val Global: Parser[List[Token], Decl] =
  (T_KWD("val") ~ GlobalIdParser ~ T_TY("Int") ~ T_OP("=") ~ NumParser) ==>  //  ints
    {case _ ~ x ~ _ ~ _ ~ y => Const(x, y): Decl } || 
  (T_KWD("val") ~ GlobalIdParser ~ T_TY("Double") ~ T_OP("=") ~ FloatParser) ==> // doubles
    {case _ ~ x ~ _ ~ _ ~ y => FConst(x, y): Decl }

lazy val Defn: Parser[List[Token], Decl] =
   (T_KWD("def") ~ IdParser ~ T_LPAREN ~ ListParser2(IdParser, TypeParser, T_COMMA) ~ T_RPAREN ~ TypeParser ~ T_OP("=") ~ Exp) ==> 
     { case _ ~ y ~ _ ~ w ~ _ ~ z ~ _ ~ r => Def(y, w, z, r): Decl } || // function def with args
   (T_KWD("def") ~ IdParser ~ T_LPAREN ~ T_RPAREN ~ TypeParser ~ T_OP("=") ~ Exp) ==>
     { case _ ~ y ~ _ ~ _ ~ z ~ _ ~ r => Def(y, List(), z, r): Decl } // function def with no args

lazy val Prog: Parser[List[Token], List[Decl]] =
  (Global ~ T_SEMI ~ Prog) ==> { case x ~ _ ~ z => x :: z : List[Decl] } ||
  (Defn ~ T_SEMI ~ Prog) ==> { case x ~ _ ~ z => x :: z : List[Decl] } ||
  (Exp ==> ((s) => List(Main(s)) : List[Decl]))


// Reading tokens and Writing parse trees

// pre-2.5.0 ammonite 
// import ammonite.ops._

// post 2.5.0 ammonite
// import os._

def parse_tks(tks: List[Token]) : List[Decl] = 
  Prog.parse_single(tks)

//@doc("Parses a file.")
@main
def main(fname: String) : Unit = {
  val tks = tokenise(os.read(os.pwd / fname))
  println(tks)
  println(parse_tks(tks))
}
// Def(top,
// List(),
// Void,
//   Sequence(
//     Call(print_int,
//         List(Call(fact,List(Num(6))))),
//     Sequence(
//       Call(print_char,List(ChConst(44))),
//       Sequence(
//         Call(print_int,List(Call(facti,List(Num(6))))),
//           Sequence(
//             Call(print_char,List(ChConst(10))),
//             Call(print_glob,List(Var(ten))))
//       )
//     )
//   )
// )