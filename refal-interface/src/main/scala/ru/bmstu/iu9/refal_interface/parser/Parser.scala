package ru.bmstu.iu9.refal_interface.parser

import ru.bmstu.iu9.refal_interface.compiler.CompilerData
import ru.bmstu.iu9.refal_interface.Grammar.DomainTag._
import ru.bmstu.iu9.refal_interface.compiler.CompilerData.UnexpectedTokenMessage
import ru.bmstu.iu9.refal_interface.parser.Tree.{Node, Nonterm, Term}

class Parser(val text: String) {
  val compiler = new CompilerData
  private val scanner = compiler scan text
  private var cur = scanner.next

  def hasErrors: Boolean = compiler.hasErrors

  def outputMessages: String = compiler.outputMessages

  private def next(): Unit = cur = scanner.next

  private def expectedError(expected: Tag): Unit = {
    compiler addMessage(cur.coords.starting, new UnexpectedTokenMessage(expected, cur.tag))
  }

  def parse(): Node = {
    parseProgram()
  }

  // Program ::= Function {Function}
  private def parseProgram(): Node = {
    val program = new Nonterm(PROGRAM)
    do {
      program addChild parseFunction()
    } while (cur.tag == IDENT)
    if (cur.tag != EOF) {
      expectedError(EOF)
    }
    program
  }

  // Function ::= IDENT Body
  private def parseFunction(): Node = {
    val function = new Nonterm(FUNCTION)
    if (cur.tag == IDENT) {
      function addChild new Term(cur)
      if (compiler.countFunc(cur.image) > 1) {
        compiler.addMessage(cur.coords.starting,
          s"Doubly defined function \'${cur.image}\'",
          isError = true)
      }
      next()
      function addChild parseBody()
    } else {
      expectedError(IDENT)
    }
    function
  }

  // Body ::= "{" Sentences "}"
  private def parseBody(): Node = {
    val body = new Nonterm(BODY)
    if (cur.tag == LBRACE) {
      body addChild new Term(cur)
      next()
      body addChild parseSentences()
      if (cur.tag == RBRACE) {
        body addChild new Term(cur)
        next()
      } else expectedError(RBRACE)
    } else expectedError(LBRACE)
    body
  }

  // Sentences ::= Sentence ";" { Sentence ";"}
  private def parseSentences(): Node = {
    val sentences = new Nonterm(SENTENCES)
    do {
      sentences addChild parseSentence()
      if (cur.tag == SEMICOLON) {
        sentences addChild new Term(cur)
        next()
      } else expectedError(SEMICOLON)
    } while (List(
        CONST,
        EVAR,
        TVAR,
        LPAREN,
        RESULT).contains(cur.tag))
    sentences
  }

  // Sentence ::= Pattern "=" RESULT
  private def parseSentence(): Node = {
    val sentence = new Nonterm(SENTENCE)
    sentence addChild parsePattern()
    if (cur.tag == RESULT) {
      sentence addChild new Term(cur)
      next()
    } else expectedError(RESULT)
    sentence
  }

  // Pattern ::= {PatternTerm}
  private def parsePattern(): Node = {
    val pattern = new Nonterm(PATTERN)
    while(List(
      CONST,
      EVAR,
      TVAR,
      LPAREN).contains(cur.tag)) {
      pattern addChild parsePatternTerm()
    }
    pattern
  }

  // PatternTerm ::= CONST | E_VARIABLE| T_VARIABLE | "(" Pattern ")"
  private def parsePatternTerm(): Node = {
    val patternTerm = new Nonterm(PATTERN_TERM)
    if (cur.tag == LPAREN) {
      patternTerm addChild new Term(cur)
      next()
      patternTerm addChild parsePattern()
      if (cur.tag == RPAREN) {
        patternTerm addChild new Term(cur)
        next()
      } else expectedError(RPAREN)
    } else {
      patternTerm addChild new Term(cur)
      next()
    }
    patternTerm
  }
}
