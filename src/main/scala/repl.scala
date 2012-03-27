import lisp.Parser
import lisp.MismatchedException

object REPL {
  val parser = new Parser

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
    parser.parser(program)
  }
}
