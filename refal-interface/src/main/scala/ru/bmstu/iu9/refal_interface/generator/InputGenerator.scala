package ru.bmstu.iu9.refal_interface.generator

import ru.bmstu.iu9.refal_interface.Grammar.DomainTag._
import ru.bmstu.iu9.refal_interface.compiler.CompilerData
import ru.bmstu.iu9.refal_interface.lexer.{ConstToken, IdentToken, SignToken, VariableToken}
import ru.bmstu.iu9.refal_interface.parser.Tree._

import scala.annotation.tailrec
import scala.collection.mutable

class InputGenerator(val compiler: CompilerData, val ast: Node) {
  private val getChildren: Node => List[Node] =
    _.asInstanceOf[Nonterm].getChildren.toList

  private val getFunctions: List[(String, List[Node])] = {
    val getFunctionName: Node => String = node =>
      compiler.nameFunction(node.asInstanceOf[Term]
        .token.asInstanceOf[IdentToken]
        .code)

    @tailrec
    def findAllByTag(tag: Tag, acc: List[Node], remain: List[Node]): List[Node] = {
      remain match {
        case Nil => acc
        case head :: tail =>
          head match {
            case nonterm: Nonterm =>
              if (nonterm.tag == tag) {
                findAllByTag(tag, acc ::: List(head), tail)
              } else {
                findAllByTag(tag, acc, tail ::: getChildren(head))
              }
            case _ => findAllByTag(tag, acc, tail)
          }
      }
    }

    findAllByTag(FUNCTION, List(), getChildren(ast))
      .map(func =>
        getFunctionName(getChildren(func).head) ->
          findAllByTag(PATTERN, List(), getChildren(func)))
  }

  private val getTasksString: List[Node] => List[String] = patterns => {
    val eVarCodes = mutable.HashMap.empty[Int, Int]
    val tVarCodes = mutable.HashMap.empty[Int, Int]
    val eVarCode: Int => Int = code => eVarCodes getOrElseUpdate(code, eVarCodes.size)
    val tVarCode: Int => Int = code => tVarCodes getOrElseUpdate(code, tVarCodes.size)
    val getShift: mutable.Map[Int, Int] => Int = m => if (m.isEmpty) 0 else m.max._1+1
    val getVarString: (mutable.Map[Int, Int], Int, String) => String = (m, limit, varType) =>
      m.filter(pair => pair._1 < limit).values.map(index => s"$varType$index").mkString("{", ",", "}")

    @tailrec
    def patternToString(sb: mutable.StringBuilder,
                        shiftE: Int,
                        shiftT: Int,
                        remain: List[Node]): (String, Int, Int) = {
      remain match {
        case Nil => (sb.toString(), getShift(eVarCodes), getShift(tVarCodes))
        case head :: tail =>
          head match {
            case _: Nonterm =>
              patternToString(sb, shiftE, shiftT, tail ::: getChildren(head))
            case term: Term =>
              term.token match {
                case const: ConstToken =>
                  if (sb.nonEmpty) sb += ' '
                  sb ++= const.name
                case paren: SignToken =>
                  sb += (if (paren.tag == LPAREN) '(' else ')')
                case variable: VariableToken =>
                  if (sb.nonEmpty) sb += ' '
                  sb ++= (
                    if (variable.tag == EVAR) {
                      s"x${eVarCode(variable.code + shiftE)}"
                    } else {
                      s"t${tVarCode(variable.code + shiftT)}"
                    })
                case _ =>
              }
              patternToString(sb, shiftE, shiftT, tail)
          }
      }
    }
    patterns.map(p =>
      patternToString(new mutable.StringBuilder(),
        getShift(eVarCodes),
        getShift(tVarCodes),
        getChildren(p)))
      .inits
      .toList
      .reverse
      .tail
      .tail // TODO: check for List.empty???
      .map(task =>
        s"""${getVarString(eVarCodes, task.last._2, "x")}
           |${getVarString(tVarCodes, task.last._3, "t")}
           |${task.map(tuple => s"\"${tuple._1}\"").init.mkString("(", ",", ")")}
           |\"${task.last._1}\"""".stripMargin)
  }

  val getFormattedData: List[List[(String, String)]] =
    getFunctions.map(func =>
      getTasksString(func._2).zipWithIndex.map(tuple => (s"${func._1}_${tuple._2}", tuple._1)))
}
