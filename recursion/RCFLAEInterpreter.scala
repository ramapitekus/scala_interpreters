// This file contains an interpreter for RCFLAE with deferred substitution and
// static scoping. The interpreter employs circular environments to implement
// recursion.
package recursion

object RCFLAEInterpreter:
  sealed abstract class RCFLAE
  case class Num(n: Int) extends RCFLAE
  case class Add(lhs: RCFLAE, rhs: RCFLAE) extends RCFLAE
  case class Sub(lhs: RCFLAE, rhs: RCFLAE) extends RCFLAE
  case class Mult(lhs: RCFLAE, rhs: RCFLAE) extends RCFLAE
  case class Let(name: String, namedExpr: RCFLAE, body: RCFLAE) extends RCFLAE
  case class Id(name: String) extends RCFLAE
  case class Fun(param: String, body: RCFLAE) extends RCFLAE
  case class If0(test: RCFLAE, posBody: RCFLAE, negBody: RCFLAE) extends RCFLAE
  case class Rec(name: String, namedExpr: RCFLAE, body: RCFLAE) extends RCFLAE
  case class App(funExpr: RCFLAE, argExpr: RCFLAE) extends RCFLAE

  type Env = scala.collection.Map[String, Val]

  sealed abstract class Val
  case class NumV(n: Int) extends Val
  case class Closure(param: String, body: RCFLAE, env: Env) extends Val

  def interp(expr: RCFLAE, env: Env = Map()): Val = expr match
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
        case _ =>
          sys.error("can only subtract numbers, but got: " + (lhsV, rhsV))
    case Mult(lhs, rhs) =>
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ => sys.error("can only multiply numbers, but got: " + (lhsV, rhsV))
    case Let(boundId, namedExpr, boundBody) =>
      interp(boundBody, env + (boundId -> interp(namedExpr, env)))
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
      val recEnv = collection.mutable.Map() ++ env
      recEnv += boundId -> interp(namedExpr, recEnv)
      interp(boundBody, recEnv)
    case App(funExpr, argExpr) =>
      val funV = interp(funExpr, env)
      funV match
        case Closure(fParam, fBody, fEnv) =>
          interp(fBody, fEnv + (fParam -> interp(argExpr, env)))
        case _ => sys.error("can only apply functions, but got: " + funV)

  given Conversion[String, RCFLAE] with
    def apply(s: String): RCFLAE = Id(s)
  given Conversion[Int, RCFLAE] with
    def apply(n: Int): RCFLAE = Num(n)
