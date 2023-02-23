package ru.bmstu.iu9

import algorithm.{Inclusion, Node}
import refal_exporter.RecursiveParser

object Main {

  def main(args: Array[String]): Unit = {
    RecursiveParser.generateTasks("input/test_01.ref")
    val patterns = Node.getPatterns("output/tasks/ScreeningTest_2")
//    println(s"${patterns.map(_.toString).mkString("\n")}")
    println(s"Alg: ${Inclusion.isSubset(patterns)}")
  }
}
