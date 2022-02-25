package arithmetic_expressions

object AEInterpreter:

  sealed abstract class AE
  case class Num(n: Int) extends AE
  case class Add(lhs: AE, rhs: AE) extends AE
  case class Sub(lhs: AE, rhs: AE) extends AE
  case class Mul(lhs: AE, rhs: AE) extends AE

  def interp(expr: AE): Int = expr match
    case Num(n)        => n
    case Add(lhs, rhs) => interp(lhs) + interp(rhs)
    case Sub(lhs, rhs) => interp(lhs) - interp(rhs)
    case Mul(lhs, rhs) => interp(lhs) * interp(rhs)

  given Conversion[Int, AE] with
    def apply(n: Int): AE = Num(n)
