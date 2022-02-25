package interpreter_types

// This file contains an interpreter for RCFLAE with functional environments.
object RCFLAEFEInterpreter:

  sealed abstract class RCFLAEFE
  case class Num(n: Int) extends RCFLAEFE
  case class Add(lhs: RCFLAEFE, rhs: RCFLAEFE) extends RCFLAEFE
  case class Sub(lhs: RCFLAEFE, rhs: RCFLAEFE) extends RCFLAEFE
  case class Mult(lhs: RCFLAEFE, rhs: RCFLAEFE) extends RCFLAEFE
  case class Let(name: String, namedExpr: RCFLAEFE, body: RCFLAEFE) extends RCFLAEFE
  case class Id(name: String) extends RCFLAEFE
  case class Fun(param: String, body: RCFLAEFE) extends RCFLAEFE
  case class If0(test: RCFLAEFE, posBody: RCFLAEFE, negBody: RCFLAEFE) extends RCFLAEFE
  case class Rec(name: String, namedExpr: RCFLAEFE, body: RCFLAEFE) extends RCFLAEFE
  case class App(funExpr: RCFLAEFE, argExpr: RCFLAEFE) extends RCFLAEFE

  type Env = String => Val
  def emptyEnv(name: String): Val = sys.error("No binding for " + name)
  def cycBindAndInterp(boundId: String, namedExpr: RCFLAEFE, env: Env): Env =
    lazy val recEnv: Env = (id: String) =>
      if (id == boundId) interp(namedExpr, recEnv)
      else env(id)
    recEnv

  sealed abstract class Val
  case class NumV(n: Int) extends Val
  case class Closure(param: String, body: RCFLAEFE, env: Env) extends Val

  def createEnv(name: String, value: Val, oldEnv: Env): Env =
    (id: String) =>
      if id == name then value
      else oldEnv(id)

  def interp(expr: RCFLAEFE, env: Env = emptyEnv): Val = expr match
    case Num(n) => NumV(n)
    case Add(lhs, rhs) =>
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _                    => sys.error("can only add numbers, but got: " + (lhsV, rhsV))
    case Sub(lhs, rhs) =>
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match
        case (NumV(n1), NumV(n2)) => NumV(n1 - n2)
        case _ => sys.error("can only subtract numbers, but got: " + (lhsV, rhsV))
    case Mult(lhs, rhs) =>
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ => sys.error("can only multiply numbers, but got: " + (lhsV, rhsV))
    case Let(boundId, namedExpr, boundBody) =>
      interp(boundBody, createEnv(boundId, interp(namedExpr, env), env))
    case Id(name)       => env(name)
    case Fun(arg, body) => Closure(arg, body, env)
    case If0(testExpr, thenExpr, elseExpr) =>
      val testV = interp(testExpr, env)
      testV match
        case NumV(n) =>
          if n == 0 then interp(thenExpr, env)
          else interp(elseExpr, env)
        case _ => sys.error("can only test numbers, but got: " + testV)
    case Rec(boundId, namedExpr, boundBody) =>
      interp(boundBody, cycBindAndInterp(boundId, namedExpr, env))
    case App(funExpr, argExpr) =>
      val funV = interp(funExpr, env)
      funV match
        case Closure(fParam, fBody, fEnv) =>
          interp(fBody, createEnv(fParam, interp(argExpr, env), fEnv))
        case _ => sys.error("can only apply functions, but got: " + funV)

  given Conversion[String, RCFLAEFE] with
    def apply(s: String): RCFLAEFE = Id(s)
  given Conversion[Int, RCFLAEFE] with
    def apply(n: Int): RCFLAEFE = Num(n)
