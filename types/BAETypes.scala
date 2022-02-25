package types

object BAETypes:

  sealed abstract class AE
  case class Num(n: Int) extends AE
  case class Bool(b: Boolean) extends AE

  case class Add(lhs: AE, rhs: AE) extends AE
  case class Sub(lhs: AE, rhs: AE) extends AE

  case class And(lhs: AE, rhs: AE) extends AE
  case class Or(lhs: AE, rhs: AE) extends AE
  case class Not(x: AE) extends AE

  case class If(c: AE, ib: AE, eb: AE) extends AE // This can go wrong!

  sealed abstract class Type
  case class TNum() extends Type
  case class TBool() extends Type

  def typeOf(e: AE): Type = e match
    case Num(n)  => TNum()
    case Bool(x) => TBool()
    case Add(lhs, rhs)
        if typeOf(lhs) == TNum() &&
          typeOf(rhs) == TNum() =>
      TNum()
    case Sub(lhs, rhs)
        if typeOf(lhs) == TNum() &&
          typeOf(rhs) == TNum() =>
      TNum()
    case And(lhs, rhs)
        if typeOf(lhs) == TBool() &&
          typeOf(rhs) == TBool() =>
      TBool()
    case Or(lhs, rhs)
        if typeOf(lhs) == TBool() &&
          typeOf(rhs) == TBool() =>
      TBool()
    case If(c, ib, eb) if typeOf(c) == TBool() =>
      (typeOf(ib), typeOf(eb)) match
        case (t1, t2) if t1 == t2 => t1
        case _                    => sys.error("Different types on branches")
    case Not(x) if typeOf(x) == TBool() => TBool()
    case _                              => sys.error("Type error!")
