package ru.bmstu.iu9.refal_interface

object Grammar {
  object DomainTag extends Enumeration {
    type Tag = Value
    val
      // TODO: move to another place
      // non-terminals
      PROGRAM,
      FUNCTION,
      BODY,
      SENTENCES,
      SENTENCE,
      PATTERN,
      PATTERN_TERM,
      // terminals
      NEWLINE,
      BLANK,
      LPAREN,
      RPAREN,
      LBRACE,
      RBRACE,
      SEMICOLON,
      CONST,
      RESULT,
      EVAR,
      TVAR,
      IDENT,
      UNKNOWN,
      EOF = Value
  }

  import Grammar.DomainTag._

  import scala.collection.immutable.SortedMap

  val LPAREN_REGEX: String = "\\("
  val RPAREN_REGEX: String = "\\)"
  val LBRACE_REGEX: String = "\\{"
  val RBRACE_REGEX: String = "\\}"
  val SEMICOLON_REGEX: String = ";"
  val IDENT_REGEX: String = "[\\w](?:[\\w-])*"
  val CONST_REGEX: String = "(?:'\\S*'(?:\\s*'\\S*')*)+"
  val EVAR_REGEX: String = "e." + IDENT_REGEX
  val TVAR_REGEX: String = "t." + IDENT_REGEX
  val NEWLINE_REGEX: String = "\\n+"
  val BLANK_REGEX: String = "[ \\t]+"
  val UNKNOWN_REGEX: String = "\\S+"
  val RESULT_REGEX: String = "=[^;]*"


  val tokenRegex: Map[Tag, String] = SortedMap(
    NEWLINE   -> NEWLINE_REGEX,
    BLANK     -> BLANK_REGEX,
    LPAREN    -> LPAREN_REGEX,
    RPAREN    -> RPAREN_REGEX,
    LBRACE    -> LBRACE_REGEX,
    RBRACE    -> RBRACE_REGEX,
    SEMICOLON -> SEMICOLON_REGEX,
    RESULT    -> RESULT_REGEX,
    CONST     -> CONST_REGEX,
    EVAR      -> EVAR_REGEX,
    TVAR      -> TVAR_REGEX,
    IDENT     -> IDENT_REGEX,
    UNKNOWN   -> UNKNOWN_REGEX
  )
}
