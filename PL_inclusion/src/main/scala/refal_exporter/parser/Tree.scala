package ru.bmstu.iu9
package refal_exporter.parser

import refal_exporter.lexer.Token

import scala.collection.mutable.ListBuffer

object Tree {
  import refal_exporter.Grammar.DomainTag._

  import scala.collection.mutable

  class Node(val tag: Tag) {
    override def toString: String = toString(Node.indent)

    def toString(indent: String): String = correctIndent(indent) + tag

    protected def correctIndent(indent: String): String = if (indent.isEmpty) Node.indent else indent
  }

  object Node {
    val indent = "────"
    val startIndent = "├───"
  }

  class Term(val token: Token) extends Node(token.tag) {
    override def toString: String = toString(Node.indent)

    override def toString(indent: String): String = correctIndent(indent) + token
  }

  class Nonterm(tag: Tag) extends Node(tag) {
    val childNode: Int => Option[Node] = index => children.lift(index)
    private val children: mutable.ListBuffer[Node] = mutable.ListBuffer.empty[Node]

    def getChildren: ListBuffer[Node] = children

    def addChild(child: Node): Unit = children.append(child)

    def hasChild: Boolean = children.nonEmpty

    override def toString(indent: String): String = {
      val ind = s"${correctIndent(indent)}${Node.indent}"
      s"${super.toString(indent)}:\n${
        if (!hasChild) s"${ind}EPS"
        else children map (child => child toString ind) mkString "\n"
      }"
    }
  }
}