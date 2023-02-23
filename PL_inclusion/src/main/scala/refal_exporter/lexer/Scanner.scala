package ru.bmstu.iu9
package refal_exporter.lexer

object Scanner {
  import refal_exporter.Grammar.DomainTag._
  import refal_exporter.Grammar.tokenRegex

  import scala.util.matching.Regex

  private val groupPattern = (name: Tag, regex: String) => s"(?<$name>$regex)"
  private val pattern: Map[Tag, String] => String = _ map (entry => groupPattern(entry._1, entry._2)) mkString "|"
  private val decomposeGroup: Regex.Match => (Tag, String, Int) = m => {
    val tag = tokenRegex.keySet find (k => m.group(k.toString) != null) getOrElse UNKNOWN
    val image = if (m.group(tag.toString) != null) m.group(tag.toString) else m.toString()
    (tag, image, image.length)
  }
}

import refal_exporter.compiler.CompilerData

class Scanner(text: String, compiler: CompilerData) extends Iterator[Token] {
  import refal_exporter.Grammar.DomainTag._
  import refal_exporter.Grammar.tokenRegex

  import Token._
  import refal_exporter.compiler.CompilerData.UnknownTokenMessage

  private val it = Scanner.pattern(tokenRegex).r findAllMatchIn text
  private val token: (Tag, String, Fragment) => Token = {
    case (IDENT, img, frag) => new IdentToken(
      compiler.codeFunction(img),
      frag)
    case (CONST, img, frag) => new ConstToken(
      CONST,
      img.replaceAll("['\\s]", ""),
      frag)
    case (RESULT, img, frag) => new ConstToken(RESULT, img, frag)
    case (EVAR, img, frag) => new VariableToken(
      EVAR,
      compiler.codeVariable(img.substring(2)),
      frag)
    case (TVAR, img, frag) => new VariableToken(
      TVAR,
      compiler.codeVariable(img.substring(2)),
      frag)
    case (UNKNOWN, img, frag) => new UnknownToken(img, frag)
    case (LPAREN, _, frag) => new SignToken(LPAREN, frag)
    case (RPAREN, _, frag) => new SignToken(RPAREN, frag)
    case (LBRACE, _, frag) => new SignToken(LBRACE, frag)
    case (RBRACE, _, frag) => new SignToken(RBRACE, frag)
    case (SEMICOLON, _, frag) => new SignToken(SEMICOLON, frag)
    case (_, _, _) => next()
  }
  private var cur = new Position(text)
  private var needEOF = true

  override def hasNext: Boolean = cur.notEOF && it.hasNext || needEOF

  override def next: Token = { // NOTE: always returns EOF if there is no next token
    if (cur.isEOF || !it.hasNext) {

      needEOF = false
      new EndToken(new Fragment(cur, cur))
    } else {
//      println(Scanner.pattern(tokenRegex).r)
      val (tag, image, len) = Scanner.decomposeGroup(it.next)
      val start = new Position(cur)
      cur = if (tag == NEWLINE) Position.yShifted(cur, len) else Position.xShifted(cur, len)
      if (tag == UNKNOWN) compiler.addMessage(start, new UnknownTokenMessage(image))
      token(tag, image, new Fragment(start, cur))
    }
  }
}
