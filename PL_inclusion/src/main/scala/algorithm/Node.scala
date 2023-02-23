package ru.bmstu.iu9
package algorithm

import algorithm.VarType.Var
import refal_exporter.RecursiveParser.read

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object VarType extends Enumeration {
  type Var = Value
  val S, T, E, SYMBOLS, EXPR, EMPTY_EXPR = Value
}

class Node(val tag: Var) {
  override def toString: String = tag.toString
}

object Node {

  def getPatterns(name: String): List[Expr] = {
    val input = read(name)
    if (input == "") {
      println("Expected non-empty refal program")
      List.empty
    } else {
      val eCountPattern = "x(\\d+)}".r
      val tCountPattern = "t(\\d+)}".r
      val patternsPattern = "\"([^\"]*?)\"".r
      val termPattern = "'(?<c>.*?)'|x(?<e>\\d+)|t(?<t>\\d+)".r

      val getFirst = (p: Regex, src: String) => p.findFirstMatchIn(src) match {
        case Some(last) => last.group(1).toInt
        case None => -1
      }
      val eCount = getFirst(eCountPattern, input) + 1
      val tCount = getFirst(tCountPattern, input) + 1

      def parse(pattern: String): Expr = {
        def addTerms(expr: Expr, p: String): Unit = {
          termPattern.findAllMatchIn(p)
            .foreach(patternMatch => {
              val (c, e, t) = (patternMatch.group("c"), patternMatch.group("e"), patternMatch.group("t"))
              if (c != null) {
                Inclusion.alphabet ++= c.toList
                expr.addChild(new Term(VarType.SYMBOLS, 0, c))
              }
              if (e != null) expr.addChild(new Term(VarType.E, e.toInt, s"x$e"))
              if (t != null) expr.addChild(new Term(VarType.T, t.toInt, s"t$t"))
            })
        }
        def getExprRange(s: String, start: Int): Int = {
          if (s.isEmpty) -1
          else {
            @scala.annotation.tailrec
            def go(position: Int, stack: List[Char]): Int = {
              if (position != start && stack.isEmpty) position
              else {
                val char = s(position)
                if (char == '(') go(position+1, char :: stack)
                else if (char == ')') go(position+1, stack.tail)
                else go(position+1, stack)
              }
            }
            go(position = start, stack = List.empty)
          }
        }
        def getDiv(pattern: String): (Int, Int) = {
          val start = pattern.indexOf("(")
          val end = if (start > -1) getExprRange(pattern, start) else -1
          (start, end)
        }
        def parseRec(p: String, expr: Expr): Unit = {
          val (start, end) = getDiv(p)
          if (start > -1) {
            addTerms(expr, p.substring(0, start))
            val newExpr = new Expr(VarType.EXPR, expr.level+1)
            expr.addChild(newExpr)
            parseRec(p.substring(start+1, end-1), newExpr)
            parseRec(p.substring(end), expr)
          } else {
            addTerms(expr, p)
          }
        }
        val root = new Expr(VarType.EXPR, 0)
        parseRec(pattern, root)
        root
      }

      patternsPattern
        .findAllMatchIn(input)
        .map(patternMatch => patternMatch.group(1).filterNot(_.isWhitespace))
        .map(pattern => parse(pattern)).toList
    }
  }

}
class Term(tag: Var, val id: Int, val src: String) extends Node(tag) {
  override def toString: String = src
}

class Expr(tag: Var, val level: Int) extends Node(tag) {
  val childNode: Int => Option[Node] = index => children.lift(index)
  private val children: mutable.ListBuffer[Node] = mutable.ListBuffer.empty[Node]
  def getChildren: ListBuffer[Node] = children
  def addChild(child: Node): Unit = children.append(child)
  def hasChild: Boolean = children.nonEmpty
  override def toString: String = s"(${children.map(_.toString).mkString(" ")})"
//  def toString(level: Int): String = {
//    val
//    s"(${children.map(_.toString).mkString(" ")})"
//  }
}


