// An LLVM Compiler for the Fun language with type checking
//
// call with 
//
//     amm fun_llvm.sc run fact.fun
//
//     amm fun_llvm.sc run defs.fun
// ...


// Types of expressions (function and local variables)
// and global variables are determined using a typing environment
// initialised before the compilation phase and type safety is verified


// Tokenizer and Parser modified for the Fun language
import $file.fun_tokens, fun_tokens._
import $file.fun_parser, fun_parser._ 


// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// Internal CPS language for FUN
type Ty = String
abstract class KExp
abstract class KVal

case class KVar(s: String) extends KVal
case class KGlobalVar(s: String) extends KVal
case class KNum(i: Int) extends KVal
case class KFNum(f: Double) extends KVal
case class KChar(c: Int) extends KVal

case class Kop(o: String, v1: KVal, v2: KVal) extends KVal
case class KCall(o: String, vrs: List[KVal]) extends KVal

case class KLet(x: String, e1: KVal, e2: KExp) extends KExp {
  override def toString = s"LET $x = $e1 in \n$e2" 
}
case class KIf(x1: String, e1: KExp, e2: KExp) extends KExp {
  def pad(e: KExp) = e.toString.replaceAll("(?m)^", "  ")

  override def toString = 
     s"IF $x1\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
}
case class KReturn(v: KVal) extends KExp


// CPS translation from Exps to KExps using a
// continuation k.
def CPS(e: Exp)(k: KVal => KExp) : KExp = e match {
  case Var(s) => k(KVar(s))
  case Num(i) => k(KNum(i))
  case FNum(d) => k(KFNum(d))
  case ChConst(c) => k(KChar(c))
  case GlobalVar(s) => {
    // In order to use a global value, first load into a local variable
    val z = Fresh("tmp")
    KLet(z, KGlobalVar(s), k(KVar(z)))
  }
  case Aop(o, e1, e2) => {
    val z = Fresh("tmp")
    CPS(e1)(y1 => 
      CPS(e2)(y2 => KLet(z, Kop(o, y1, y2), k(KVar(z)))))
  }
  case If(Bop(o, b1, b2), e1, e2) => {
    val z = Fresh("tmp")
    CPS(b1)(y1 => 
      CPS(b2)(y2 => 
        KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1)(k), CPS(e2)(k)))))
  }
  case Call(name, args) => {
    def aux(args: List[Exp], vs: List[KVal]) : KExp = args match {
      case Nil => {
          val z = Fresh("tmp")
          KLet(z, KCall(name, vs), k(KVar(z)))
      }
      case e::es => CPS(e)(y => aux(es, vs ::: List(y)))
    }
    aux(args, Nil)
  }
  case Sequence(e1, e2) => 
    CPS(e1)(_ => CPS(e2)(y2 => k(y2)))
}

//initial continuation
def CPSi(e: Exp) = CPS(e)(KReturn)


// convenient string interpolations 
// for instructions, labels and methods
import scala.language.implicitConversions
import scala.language.reflectiveCalls

implicit def sring_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
    def m(args: Any*): String = sc.s(args:_*) ++ "\n"
}

// mathematical and boolean operations
def compile_iop(op: String) = op match { // integer operations
  case "+" => "add i32 "
  case "*" => "mul i32 "
  case "-" => "sub i32 "
  case "/" => "sdiv i32 "
  case "%" => "srem i32 "
  case "==" => "icmp eq i32 "
  case "!=" => "icmp ne i32 "
  case "<=" => "icmp sle i32 "     // signed less or equal
  case "<"  => "icmp slt i32 "     // signed less than

}
def compile_dop(op: String) = op match { // double operations
  case "+" => "fadd double "
  case "*" => "fmul double "
  case "-" => "fsub double "
  case "==" => "fcmp oeq double "
  case "!=" => "fcmp one double "
  case "<=" => "fcmp ole double "
  case "<" => "fcmp olt double "
}

/*
* This function recursively determines the type of a value KVal using
* the typing environment TyEnv which contains the types of every function, global
* and local variable declared so far in the program.
*
* Returns a type from ["Int", "Double", "Void", "Bool"]
*/
def typ_val(v: KVal, env: TyEnv) : String = v match {
  case KNum(_) => "Int"
  case KVar(s) => env(s)
  case KFNum(_) => "Double"
  case KChar(_) => "Int"
  case KGlobalVar(s) => env(s)
  case Kop(op, x1, x2) => {
    if (op == "==" || op == "<=" || op == ">=" || op == "!=") "Bool"
    else {
      val t1 = typ_val(x1, env)
      val t2 = typ_val(x2, env)
      // Type safety during arithmetic operation
      if (t1 == t2) t1
      else {
        println(s"Typing error during ${op} operation: ${x1} and ${x2} have different types ${t1} and ${t2}"); sys.exit(-1)
      }
    }
  }
  case KCall(x1, args) => env(x1)
}

// compile K values and returns llvm-ir string
def compile_val(v: KVal, env: TyEnv) : String = v match {
  case KNum(i) => s"$i"
  case KVar(s) => s"%$s"
  case KFNum(d) => s"$d"
  case KChar(c) => s"$c"
  case KGlobalVar(s) => {
    val globType = fun2llvmTypes(env(s))
    s"load ${globType}, ${globType}* @$s"
  }
  case Kop(op, x1, x2) => 
    val str = (x: String) => s"${x} ${compile_val(x1,env)}, ${compile_val(x2,env)}"
    // Select operator based on type of operands
    if (typ_val(x1, env) == "Int") str(compile_iop(op))
    else if (typ_val(x2, env) == "Double" && op != "/") str(compile_dop(op)) // double division unsupported
    else {
      println("Typing Error: Cannot perform arithmetic on non-Int/Double type"); sys.exit(-1)
    }
  case KCall(x1, args) => {
    val func_type = fun2llvmTypes(env(x1))
    
    // Create list of tuples of the form (function_arg, function_arg_type) to generate an argument string
    val arg_and_argtypes = args zip args.map(arg => fun2llvmTypes(typ_val(arg, env)))
    val argString = arg_and_argtypes.map(arg => 
      s"${arg._2} ${compile_val(arg._1,env)}").mkString(", ")
    
    s"call ${func_type} @$x1(${argString})"
  }
}


/*
 * Compiles K expressions and returns llvm-ir string
 *
 * Note: When compile_Exp is called for Defn's, currentFuncType set to 
 * return type of the Defn, otherwise, set to empty string
 */
def compile_exp(a: KExp, env: TyEnv, currentFuncType: String) : String = a match {
  case KReturn(v) => {
    val val_type = typ_val(v, env)
    // Type safety check for Defns only where currentFuncType is not an empty string
    if (currentFuncType != ""){
      // Check if return val type is same as return type of the Defn
      if (val_type != currentFuncType) {
        println(s"Typing Error: Value ${v} does not match function return type of ${currentFuncType}"); 
        sys.exit(-1)
      }
    }
    // Void type case
    if (val_type == "Void") i"ret void"
    // Other types
    else i"ret ${fun2llvmTypes(typ_val(v,env))} ${compile_val(v, env)}"
  }
  case KLet(x, v, e) => {
    // Determine type of KVal v in the current type environment and assign
    // x to have the same type as v in the env
    val vType = typ_val(v, env)
    val new_env : TyEnv = env + (x -> vType)

    // Compile the KVal v and KExp e
    val comp_val = compile_val(v,new_env)
    val comp_exp = compile_exp(e, new_env, currentFuncType)
    if (vType == "Void") i"${comp_val}" ++ comp_exp
    else i"%$x = ${comp_val}" ++ comp_exp
  }
  case KIf(x, e1, e2) => {
    val if_br = Fresh("if_branch")
    val else_br = Fresh("else_branch")
    i"br i1 %$x, label %$if_br, label %$else_br" ++
    l"\n$if_br" ++
    compile_exp(e1, env, currentFuncType) ++
    l"\n$else_br" ++ 
    compile_exp(e2, env, currentFuncType)
  }
}

// Fun to LLVM-IR types
val fun2llvmTypes = Map("Int" -> "i32", "Double" -> "double", 
  "Void" -> "void", "Char" -> "i8")
type TyEnv = Map[String,String]

// compile function for declarations and main
def compile_decl(resultAndGlobalEnv: (String, TyEnv), d: Decl) : (String, TyEnv) = d match {
  case Def(name, args, ty, body) => {
    val globalEnvSoFar = resultAndGlobalEnv._2
    val newGlobalEnv = globalEnvSoFar + (name -> ty) // record function signature in type env

    val argString = args.map(arg => s"${fun2llvmTypes(arg._2)} %${arg._1}").mkString(", ")
    val argsEnv = args.toMap
    val argsAndGlobalTypes : TyEnv = argsEnv ++ newGlobalEnv
    val cps = CPSi(body)
    // println(body)
    // println()
    // println(cps)
    // println()

    val resultStringSoFar = resultAndGlobalEnv._1
    val defnString = m"define ${fun2llvmTypes(ty)} @$name (${argString}) {" ++
    compile_exp(cps, argsAndGlobalTypes, ty) ++
    m"}\n"

    // Combine newly generated llvm-ir string with what we have so far
    val newResult = resultStringSoFar + defnString

    // Return back to accumulator
    (newResult,newGlobalEnv)
  }
  case Const(name, value) => {
    val globalEnvSoFar = resultAndGlobalEnv._2
    val newGlobalEnv = globalEnvSoFar + (name -> "Int")
    (resultAndGlobalEnv._1 + m"@$name = global i32 $value \n", newGlobalEnv)
  }
  case FConst(name, value) => {
    val globalEnvSoFar = resultAndGlobalEnv._2
    val newGlobalEnv = globalEnvSoFar + (name -> "Double")
    (resultAndGlobalEnv._1 + m"@$name = global double $value \n", newGlobalEnv)
  }
  // Global typing environment does not change in the main section
  case Main(body) => {
    val resultStringSoFar = resultAndGlobalEnv._1
    val mainString = m"define i32 @main() {" ++
    compile_exp(CPS(body)(_ => KReturn(KNum(0))), resultAndGlobalEnv._2, "") ++
    m"}\n"
    val newResult = resultStringSoFar + mainString

    (newResult,resultAndGlobalEnv._2)
  }
}

// main compiler functions
def compile(prog: List[Decl]) : String = {
  // Create initial typing environment of build in functions by giving them
  // all the Void type
  val initEnv : TyEnv = Map("new_line" -> "Void",
                         "print_star" -> "Void", 
                         "print_space" -> "Void", 
                         "skip" -> "Void",
                         "print_int" -> "Void", 
                         "print_char" -> "Void")
  
  // Generate new llvm-ir strings using previously created strings and 
  // a global typing environment
  val finalResult = prog.foldLeft(("",initEnv))(compile_decl)._1
  // println(finalResult);


  prelude ++ finalResult
}

// Note: contains functions taken from sqr.ll and the print_char 
// function which takes an integer input and prints out ASCII equivalent
// character
val prelude = """

declare i32 @printf(i8*, ...)

@.str = private constant [3 x i8] c"%d\00"
@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"
@.str_char = private constant [3 x i8] c"%c\00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @skip() #0 {
  ret void
}

define void @print_int(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.str, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
   ret void
}

define void @print_char(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.str_char, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
   ret void
}

; END OF BUILD-IN FUNCTIONS (prelude)
"""

// pre-2.5.0 ammonite 
// import ammonite.ops._

// post 2.5.0 ammonite
// import os._


@main
def main(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    val ast = parse_tks(tks)
    println(ast)
    println(compile(ast))
}

@main
def write(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    val ast = parse_tks(tks)
    val code = compile(ast)
    os.write.over(os.pwd / (file ++ ".ll"), code)
}

@main
def run(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    write(fname)  
    os.proc("llc", "-filetype=obj", file ++ ".ll").call()
    os.proc("gcc", file ++ ".o", "-o", file ++ ".bin").call()
    os.proc(os.pwd / (file ++ ".bin")).call(stdout = os.Inherit)
    println(s"done.")
}

