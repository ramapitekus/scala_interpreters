package small_step

object AESmallStepInterpreter:

  sealed abstract class AE

  // case class BinOp(lhs: AE, rhs: AE) extends AE
  case class Add(lhs: AE, rhs: AE) extends AE
  case class Sub(lhs: AE, rhs: AE) extends AE
  // case class Mul(lhs: AE, rhs: AE) extends AE

  case class Num(n: Int) extends AE with Val
  case class Hole() extends AE with Val
  trait Val

  def interp(expr: AE): Int =
    println(expr)

    val HoledExpr(exprInHole, exprWithHole) = findNext(expr)
    val value = reduce(exprInHole)
    val newExpr = subst(exprWithHole, value)

    newExpr match
      case Num(n) => println(n); n
      case e: AE  => interp(e)

  def subst(expr: AE, value: AE): AE = expr match
    case Num(n)        => expr
    case Add(lhs, rhs) => Add(subst(lhs, value), subst(rhs, value))
    case Sub(lhs, rhs) => Sub(subst(lhs, value), subst(rhs, value))
    case Hole()        => value

  def reduce(e: AE) = e match
    case Num(n)              => Num(n)
    case Add(Num(n), Num(m)) => Num(n + m)
    case Sub(Num(n), Num(m)) => Num(n - m)

  /*

         e1 -> e1'
  ----------------------
    e1 + e2 -> e1' + e2


         e -> e'
  ----------------------
    v + e -> v + e'

   */

  sealed case class HoledExpr(exprInHole: AE, exprWithHole: AE)
  def findNext(e: AE): HoledExpr = e match
    case Add(Num(n), Num(m)) => HoledExpr(Add(Num(n), Num(m)), Hole())
    case Add(Num(n), rhs) =>
      val HoledExpr(exprInHole, exprWithHole) = findNext(rhs)
      HoledExpr(exprInHole, Add(Num(n), exprWithHole))
    case Add(lhs, rhs) =>
      val HoledExpr(exprInHole, exprWithHole) = findNext(lhs)
      HoledExpr(exprInHole, Add(exprWithHole, rhs))
    case Sub(Num(n), Num(m)) => HoledExpr(Sub(Num(n), Num(m)), Hole())
    case Sub(Num(n), rhs) =>
      val HoledExpr(exprInHole, exprWithHole) = findNext(rhs)
      HoledExpr(exprInHole, Sub(Num(n), exprWithHole))
    case Sub(lhs, rhs) =>
      val HoledExpr(exprInHole, exprWithHole) = findNext(lhs)
      HoledExpr(exprInHole, Sub(exprWithHole, rhs))

  given Conversion[Int, AE] with
    def apply(n: Int): AE = Num(n)
