package first_order_functions

object CF1LAEStaticInterpreter:

  sealed abstract class CF1LAE
  case class Num(n: Int) extends CF1LAE
  case class Add(lhs: CF1LAE, rhs: CF1LAE) extends CF1LAE
  case class Sub(lhs: CF1LAE, rhs: CF1LAE) extends CF1LAE
  case class Mult(lhs: CF1LAE, rhs: CF1LAE) extends CF1LAE
  case class Let(name: String, namedExpr: CF1LAE, body: CF1LAE) extends CF1LAE
  case class If0(test: CF1LAE, posBody: CF1LAE, negBody: CF1LAE) extends CF1LAE
  case class Id(name: String) extends CF1LAE
  case class App(funName: String, arg: CF1LAE) extends CF1LAE

  case class FunDef(argName: String, body: CF1LAE)

  type FunDefs = Map[String, FunDef]
  type SubRep = Map[String, Int]

  def interp(expr: CF1LAE, funDefs: FunDefs, subRep: SubRep): Int = expr match
    case Num(n) => n
    case Add(lhs, rhs) =>
      interp(lhs, funDefs, subRep) + interp(rhs, funDefs, subRep)
    case Sub(lhs, rhs) =>
      interp(lhs, funDefs, subRep) - interp(rhs, funDefs, subRep)
    case Mult(lhs, rhs) =>
      interp(lhs, funDefs, subRep) * interp(rhs, funDefs, subRep)
    case Let(boundId, namedExpr, boundBody) =>
      val newSubRep = subRep + (boundId -> interp(namedExpr, funDefs, subRep))
      interp(boundBody, funDefs, newSubRep)
    case If0(testExpr, thenExpr, elseExpr) =>
      val testV = interp(testExpr, funDefs, subRep)
      if testV == 0 then interp(thenExpr, funDefs, subRep)
      else interp(elseExpr, funDefs, subRep)

    case Id(name) => subRep(name)
    case App(funName, argExpr) =>
      funDefs(funName) match
        case FunDef(argName, body) =>
          val funSubRep = Map(argName -> interp(argExpr, funDefs, subRep))
          interp(body, funDefs, funSubRep)

  given Conversion[String, CF1LAE] with
    def apply(s: String): CF1LAE = Id(s)
  given Conversion[Int, CF1LAE] with
    def apply(n: Int): CF1LAE = Num(n)
