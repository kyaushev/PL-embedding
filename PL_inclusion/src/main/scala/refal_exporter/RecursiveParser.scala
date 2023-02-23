package ru.bmstu.iu9
package refal_exporter

import refal_exporter.generator.InputGenerator
import refal_exporter.parser.Parser
import refal_exporter.parser.Tree.Node

import java.io._
import scala.reflect.io.Directory
import scala.util.Try

object RecursiveParser {
  val dir = "output"
  val task_dir = "tasks"
  val error = "error.txt"
  val tree = "tree.txt"
  val path: List[String] => String = list => list.mkString("/")
  val errorDir: String = path(List(dir, error))
  val treeDir: String = path(List(dir, tree))
  val taskDir: String = path(List(dir, task_dir))

  def read(fileName: String): String = {
    Try {
      val bufferedSource = scala.io.Source.fromFile(fileName)
      val text = bufferedSource.getLines().mkString
      bufferedSource.close()
      text
    }.toEither match {
      case Left(e) =>
        println(s"File read error: $e")
        ""
      case Right(text) =>
        println(s"\'$fileName\' file read successfully")
        text
    }
  }

  def write(fileName: String, text: String): Unit = {
    Try {
      val bufferedPrintWriter = new BufferedWriter(new FileWriter(new File(fileName)))
      bufferedPrintWriter.write(text)
      bufferedPrintWriter.close()
    }.toEither match {
      case Left(e) =>
        println(s"File write error: $e")
      case Right(_) =>
        println(s"\'$fileName\' file written successfully")
    }
  }
  def generateTasks(name: String): Unit = {
    val refalProgram = read(name)

    if (refalProgram == "") {
      println("Expected non-empty refal program")
    } else {
      val parser = new Parser(refalProgram)
      val ast = parser.parse()

      new Directory(new File(taskDir)).deleteRecursively()

      if (parser.compiler.hasErrors) {
        write(errorDir, parser.compiler.outputMessages)
        write(treeDir, "")
      } else {
        write(errorDir, "")
        write(treeDir, "Tree:\n" + ast.toString(Node.startIndent))
        if (new File(taskDir).mkdirs) {
          val generator = new InputGenerator(parser.compiler, ast)
          generator.getFormattedData.foreach(function =>
            function.foreach(task =>
              write(path(List(taskDir, task._1)), task._2))
          )
        }
      }
    }
  }
}
