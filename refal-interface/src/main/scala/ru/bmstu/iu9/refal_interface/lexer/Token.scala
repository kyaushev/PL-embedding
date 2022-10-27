package ru.bmstu.iu9.refal_interface.lexer

object Token {

  class Position(val text: String, val line: Int, val pos: Int, val index: Int) extends Ordered[Position] {
    val isEOF: Boolean = index == text.length
    val notEOF: Boolean = !isEOF

    def this(text: String) = this(text, 1, 1, 0)

    def this(p: Position) = this(p.text, p.line, p.pos, p.index)

    override def compare(that: Position): Int = this.index compare that.index

    override def toString: String = s"${(line, pos)}"

  }

  class Fragment(val starting: Position, val following: Position) {
    override def toString: String = s"$starting - $following"
  }

  object Position {
    def xShifted(p: Position, value: Int) = new Position(p.text, p.line, p.pos + value, p.index + value)

    def yShifted(p: Position, value: Int) = new Position(p.text, p.line + value, 1, p.index + value)
  }

}

import ru.bmstu.iu9.refal_interface.Grammar.DomainTag._
import Token.Fragment

class Token(val tag: Tag, val coords: Fragment) {
  var image: String = tag.toString
  override def toString: String = (tag, coords).toString()
}

class IdentToken(val code: Int, coords: Fragment)
  extends Token(IDENT, coords) {

  private def canEqual(a: Any) = a.isInstanceOf[IdentToken]

  override def equals(that: Any): Boolean =
    that match {
      case that: IdentToken => that.canEqual(this) && this.code == that.code
      case _ => false
    }

  override def hashCode: Int = code

  override def toString: String = super.toString + ": " + code
}

class ConstToken(tag: Tag, val name: String, coords: Fragment)
  extends Token(tag, coords) {
  image = name
  override def toString: String = super.toString + ": '" + name + "'"
}

class VariableToken(tag: Tag, val code: Int, coords: Fragment)
  extends Token(tag, coords) {

  private def canEqual(a: Any) = a.isInstanceOf[VariableToken]

  override def equals(that: Any): Boolean =
    that match {
      case that: VariableToken => this.tag == that.tag &&
        this.canEqual(that) &&
        this.code == that.code
      case _ => false
    }

  override def hashCode: Int = code

  override def toString: String = super.toString + ": " + code
}

class SignToken(tag: Tag, coords: Fragment) extends Token(tag, coords)

class UnknownToken(val value: String, coords: Fragment)
  extends Token(UNKNOWN, coords) {
  image = value
}

class EndToken(coords: Fragment) extends Token(EOF, coords)
