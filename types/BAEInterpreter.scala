package types

object BAEInterpreter:

  sealed abstract class EXP
  sealed abstract class AE extends EXP
  sealed abstract class BE extends EXP

  sealed case class Num(n: Int) extends AE
  sealed case class Bool(b: Boolean) extends BE

  sealed case class Add(lhs: AE, rhs: AE) extends AE

  sealed case class And(lhs: BE, rhs: BE) extends BE

  sealed case class IfA(c: BE, ib: AE, eb: AE) extends AE
  sealed case class IfB(c: BE, ib: BE, eb: BE) extends BE

  def interpretAE(e: AE): Int = e match
    case Num(n)        => n
    case Add(lhs, rhs) => interpretAE(lhs) + interpretAE(rhs)
    case IfA(c: BE, ib: AE, eb: AE) =>
      if interpretBE(c) then interpretAE(ib)
      else interpretAE(eb)

  def interpretBE(e: BE): Boolean = e match
    case Bool(x)       => x
    case And(lhs, rhs) => interpretBE(lhs) && interpretBE(rhs)
    case IfB(c: BE, ib: BE, eb: BE) =>
      if interpretBE(c) then interpretBE(ib)
      else interpretBE(eb)

  def interpretEXP(e: EXP): EXP = e match
    case e: BE => interpretBE(e)
    case e: AE => interpretAE(e)
    case _     => sys.error("Error!")

  given Conversion[Int, AE] with
    def apply(n: Int): AE = Num(n)
  given Conversion[Boolean, BE] with
    def apply(b: Boolean): BE = Bool(b)
