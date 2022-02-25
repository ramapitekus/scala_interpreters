package let

object LAEInterpreter:
  sealed abstract class LAE
  case class Num(n: Int) extends LAE
  case class Add(lhs: LAE, rhs: LAE) extends LAE
  case class Sub(lhs: LAE, rhs: LAE) extends LAE
  case class Let(name: String, namedExpr: LAE, body: LAE) extends LAE
  case class Id(name: String) extends LAE

  def subst(expr: LAE, substId: String, value: LAE): LAE = expr match
    case Num(n) => expr

    case Add(lhs, rhs) =>
      Add(subst(lhs, substId, value), subst(rhs, substId, value))

    case Sub(lhs, rhs) =>
      Sub(subst(lhs, substId, value), subst(rhs, substId, value))

    case Let(boundId, namedExpr, boundExpr) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      if boundId == substId then Let(boundId, substNamedExpr, boundExpr)
      else Let(boundId, substNamedExpr, subst(boundExpr, substId, value))

    case Id(name) =>
      if substId == name then value
      else expr

  def eagerCalc(expr: LAE): Int = expr match
    case Num(n)        => n
    case Add(lhs, rhs) => eagerCalc(lhs) + eagerCalc(rhs)
    case Sub(lhs, rhs) => eagerCalc(lhs) - eagerCalc(rhs)
    case Let(boundId, namedExpr, boundExpr) =>
      eagerCalc(subst(boundExpr, boundId, Num(eagerCalc(namedExpr))))
    case Id(name) => sys.error("found unbound id " + name)

  def lazyCalc(expr: LAE): Int = expr match
    case Num(n)        => n
    case Add(lhs, rhs) => lazyCalc(lhs) + lazyCalc(rhs)
    case Sub(lhs, rhs) => lazyCalc(lhs) - lazyCalc(rhs)
    case Let(boundId, namedExpr, boundExpr) =>
      lazyCalc(subst(boundExpr, boundId, namedExpr))
    case Id(name) => sys.error("found unbound id " + name)

  given Conversion[String, LAE] with
    def apply(s: String): LAE = Id(s)
  given Conversion[Int, LAE] with
    def apply(n: Int): LAE = Num(n)
