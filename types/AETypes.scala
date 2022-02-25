package types

object AETypes:

  sealed abstract class AE
  case class Num(n: Int) extends AE
  case class Add(lhs: AE, rhs: AE) extends AE
  case class Sub(lhs: AE, rhs: AE) extends AE

  sealed abstract class Type
  case class TNum() extends Type

  def typeOf(e: AE): Type = e match
    case Num(n) => TNum()
    case Add(lhs, rhs)
        if typeOf(lhs) == TNum() &&
          typeOf(rhs) == TNum() =>
      TNum()
    case Sub(lhs, rhs)
        if typeOf(lhs) == TNum() &&
          typeOf(rhs) == TNum() =>
      TNum()
    case _ => sys.error("Type error!")
