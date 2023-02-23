package ru.bmstu.iu9
package refal_exporter.compiler

import refal_exporter.compiler.CompilerData.Message
import refal_exporter.lexer.Scanner
import refal_exporter.lexer.Token.Position

object CompilerData {
  import refal_exporter.Grammar.DomainTag._

  class UnexpectedTokenMessage(val expected: Tag, val given: Tag)
    extends Message(s"expected token: $expected, given: $given", isError = true)

  class UnknownTokenMessage(val image: String)
    extends Message("unknown token: $image", isError = true)

  protected class Message(val text: String, val isError: Boolean)
}

class CompilerData {
  import scala.collection.mutable

  val scan: String => Scanner = new Scanner(_, this)

  val codeFunction: String => Int = name => {
    val code = functionCodes getOrElseUpdate(name, functionCodes.size)
    val count = functionCount getOrElseUpdate (code, 0)
    functionCount += (code -> (count+1))
    code
  }
  val countFunction: Int => Int = code => functionCount getOrElseUpdate (code, 1)
  val countFunc: String => Int = name => countFunction(functionCodes getOrElseUpdate(name, functionCodes.size))
  val codeVariable: String => Int = name => variableCodes getOrElseUpdate(name, variableCodes.size)
  val nameFunction: Int => String = code => (functionCodes find (_._2 == code)).get._1
  val nameVariable: Int => String = code => (variableCodes find (_._2 == code)).get._1

  private val messages = mutable.TreeMap.empty[Position, Message]
  private val functionCodes = mutable.HashMap.empty[String, Int]
  private val functionCount = mutable.HashMap.empty[Int, Int]
  private val variableCodes = mutable.HashMap.empty[String, Int]

  def hasErrors: Boolean = {
    messages.nonEmpty
  }

  def addMessage(position: Position, text: String, isError: Boolean): Unit = {
    messages(position) = new Message(text, isError)
  }

  def addMessage(position: Position, msg: Message): Unit = messages(position) = msg

  def outputMessages: String = messages map (e => (if (e._2.isError) "Error" else "Warning") + s" at ${e._1}: ${e._2.text}") mkString "\n"
}
