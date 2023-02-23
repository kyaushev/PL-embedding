package ru.bmstu.iu9
package algorithm

import dregex.Regex

import java.util
import scala.collection.mutable

object Inclusion {
  val alphabet = mutable.Set.empty[Char]
  val h: Map[VarType.Value, String] = Map(VarType.S -> "a", VarType.EMPTY_EXPR -> "b", VarType.EXPR -> "c")
  var ts: List[String] = List.empty

  def getDepth(expr: Expr): Int = {
    (expr.level ::
      expr.getChildren.toList
        .filter(node => node.isInstanceOf[Expr])
        .map(node => node.asInstanceOf[Expr])
        .map(node => getDepth(node))).max
  }

  def generateTs(level: Int): List[String] = {
    val x = (alphabet ++ h(VarType.S)).mkString("|")

    def exprRec(level: Int): List[String] = {
      if (level == 0) {
        List(h(VarType.EXPR))
      } else {
        val follow = exprRec(level - 1)
        s"(\\((${List(x, follow.head).mkString("|")})*\\))" :: follow
      }
    }

    exprRec(level).map(r => s"(${List(x, r).mkString("|")})").reverse
  }

  def generateRegex(expr: Expr, limit: Int): String = {
    s"(${
      expr.getChildren.toList
        .map {
          case term: Term => term.tag match {
            case VarType.SYMBOLS => term.src
            case VarType.S => h(VarType.S)
            case VarType.T => ts(expr.level)
            case VarType.E => ts(expr.level) + "*"
            case _ => ""
          }
          case node: Expr if node.level >= limit => h(VarType.EXPR)
          case node: Expr => s"\\(${generateRegex(node, limit)}\\)"
          case _ => ""
        }.mkString("")
    })"
  }
  import scala.collection.JavaConverters._
  def isSubset(patterns: List[Expr]): Boolean = {
    def subsetRec(Pi: List[Expr], Px: Expr, limit: Int, depth: Int): Boolean = {
      val PiReg = Pi.map(expr => generateRegex(expr, limit)).mkString("|")
      val PxReg = generateRegex(Px, limit)

      val PiRegList = Pi.map(expr => generateRegex(expr, limit))
      val l: List[String] = PiRegList.appended(PxReg)
      val r = Regex.compile(l.asJava)
      val x = r.get(r.size()-1)
      val v = r.subList(0, r.size()-1)
      v.forEach(reg => println(x.isSubsetOf(reg)))

      val regex = Regex.compile(util.Arrays.asList(PiReg, PxReg))
      val res = regex.get(1).isSubsetOf(regex.get(0))
//      println(s"\tL( $Px ) ⊆ (${Pi.map(e => s"L( $e )").mkString(" ⋃ ")})")
//      println(s"\t$PxReg ⊆ $PiReg")
//      println(s"\t$res")
      res &&
        ((limit == depth) || subsetRec(Pi, Px, limit + 1, depth))
    }
    alphabet += 'B'
    val reversed = patterns.reverse
    val pi = reversed.tail.reverse
    val px = reversed.head
    val depth = List(pi.map(getDepth).max, getDepth(px)).min
    ts = generateTs(depth)

    subsetRec(pi, px, 0, depth)
  }
}
