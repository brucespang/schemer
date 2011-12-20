import lisp.{Lexer, Parser, Interpreter}
import lisp.MismatchedException

object REPL {
  val lexer = new Lexer
  val parser = new Parser
  val interpreter = new Interpreter

  def main(args: Array[String]):Unit = {
    if(args.length > 0) {
      args.foreach { file =>
        val source = scala.io.Source.fromFile(file)
        val program = source.mkString
        source.close()
        run(program)
      }
    } else {
      val program = io.Source.stdin.getLines.mkString
      run(program)
    }
  }

  def run(program:String):Unit = {
    interpreter.evalSeq(parser.parse(lexer.lex(program)))
  }
}
