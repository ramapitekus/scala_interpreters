package forth

import scala.collection.immutable.List

// An immutable Stack
class Stack[A](list: List[A] = List()) {
  def push(x: A): Stack[A] = new Stack(x :: list)
  def pop: (A, Stack[A]) = (list.head, new Stack(list.tail))
}

object ForthInterpreter:

  type ForthProgr = List[Token]
  type ForthStack = Stack[Token]

  sealed abstract class Token
  case class Num(n: Int) extends Token
  case class Bool(b: Boolean) extends Token
  // case class Word() extends Token
  case class Sum() extends Token
  case class Diff() extends Token
  case class If(ithen: ForthProgr, ielse: ForthProgr) extends Token

  def interp(program: ForthProgr, stack: ForthStack = new ForthStack()): ForthStack =
    program.foldLeft(stack)((s, x) => {
      x match
        case n: Num  => s.push(n)
        case b: Bool => s.push(b)
        case v: Sum =>
          val (Num(n1), s1) = s.pop
          val (Num(n2), s2) = s1.pop
          s.push(n1 + n2)
        case v: Diff =>
          val (Num(n1), s1) = s.pop
          val (Num(n2), s2) = s1.pop
          s.push(n1 - n2)
        case If(ithen, ielse) =>
          s.pop match
            case (Bool(true), s1)  => interp(ithen, stack)
            case (Bool(false), s1) => interp(ielse, stack)
    })

  given Conversion[Int, Token] with
    def apply(n: Int): Token = Num(n)
  given Conversion[Boolean, Token] with
    def apply(b: Boolean): Token = Bool(b)
