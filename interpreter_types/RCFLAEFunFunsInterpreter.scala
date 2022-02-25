package interpreter_types

// This file contains an interpreter for RCFLAE with functions and environments
// represented by Scala functions.

object RCFLAEFunFunsInterpreter:

  sealed abstract class RCFLAEFunFuns
  case class Num(n: Int) extends RCFLAEFunFuns
  case class Add(lhs: RCFLAEFunFuns, rhs: RCFLAEFunFuns) extends RCFLAEFunFuns
  case class Sub(lhs: RCFLAEFunFuns, rhs: RCFLAEFunFuns) extends RCFLAEFunFuns
  case class Mult(lhs: RCFLAEFunFuns, rhs: RCFLAEFunFuns) extends RCFLAEFunFuns
  case class Let(name: String, namedExpr: RCFLAEFunFuns, body: RCFLAEFunFuns) extends RCFLAEFunFuns
  case class Id(name: String) extends RCFLAEFunFuns
  case class Fun(param: String, body: RCFLAEFunFuns) extends RCFLAEFunFuns
  case class If0(test: RCFLAEFunFuns, posBody: RCFLAEFunFuns, negBody: RCFLAEFunFuns)
      extends RCFLAEFunFuns
  case class Rec(name: String, namedExpr: RCFLAEFunFuns, body: RCFLAEFunFuns) extends RCFLAEFunFuns
  case class App(funExpr: RCFLAEFunFuns, argExpr: RCFLAEFunFuns) extends RCFLAEFunFuns

  type Env = String => Val

  def emptyEnv(name: String): Val = sys.error("No binding for " + name)

  def createEnv(name: String, value: Val, oldEnv: Env): Env =
    (id: String) =>
      if id == name then value
      else oldEnv(id)

  def cycBindAndInterp(boundId: String, namedExpr: RCFLAEFunFuns, env: Env): Env =
    lazy val recEnv: Env = (id: String) =>
      if id == boundId then interp(namedExpr, recEnv)
      else env(id)
    recEnv

  sealed abstract class Val
  case class NumV(n: Int) extends Val
  // It contains a Scala closure
  case class Closure(f: Val => Val) extends Val

  def interp(expr: RCFLAEFunFuns, env: Env = emptyEnv): Val = expr match
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
    case Id(name) => env(name)
    // Use Scala closures to save the closure
    case Fun(arg, body) => Closure(argValue => interp(body, createEnv(arg, argValue, env)))
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
      val argV = interp(argExpr, env)
      funV match
        // Apply the Scala closure saved before
        case Closure(f) => f(argV)
        case _          => sys.error("can only apply functions, but got: " + funV)

  given Conversion[String, RCFLAEFunFuns] with
    def apply(s: String): RCFLAEFunFuns = Id(s)
  given Conversion[Int, RCFLAEFunFuns] with
    def apply(n: Int): RCFLAEFunFuns = Num(n)
