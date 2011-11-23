import lisp.{Lexer, Parser, Interpreter}
import lisp.MismatchedException

object REPL {
  val lexer = new Lexer
  val parser = new Parser
  val interpreter = new Interpreter

  def main(args: Array[String]):Unit = {
    if(args.length > 0)
      epl(args.mkString(" "))
    else
      run()
  }

  def run(currentCommand:String=""):Unit = {
    val prompt = if(currentCommand.length > 0) "  | " else ">> "
    val command = readLine(prompt)

    try {
      println(epl(command + currentCommand))
      run()
    } catch {
      case e:MismatchedException => run(command + currentCommand)
    }
  }

  protected def epl(command:String) = {
    val tokens = lexer.lex(command)
    val ast = parser.parse(tokens)

    interpreter.evalSeq(ast)
  }
}
