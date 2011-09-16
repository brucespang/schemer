package lisp

import lisp.tokens._
import lisp.Parser.AST

class Interpreter {

  class Environment(parent:Environment=Root, val bindings:Map[WordToken, Any] = Map()) {
    def extend(bindings:Map[WordToken, Any]):Environment = new Environment(this, bindings)

    def lookup(token:WordToken):Any = bindings(token)
    def lookup(token:TrueToken):TrueToken = token
    def lookup(token:FalseToken):FalseToken = token
  }

  object Root extends Environment {
    protected var bindings = Map[String,Any](
      "add" -> '+
    )
  }

  case class Lambda(val args:List[WordToken], val body:AST, val env:Environment) {
    def apply(values:List[Token]) = {
      if(args.length != values.length)
        throw new Exception

      // Combine each arg with its respective value and convert them to a map
      val bindings = args.zip(values).toMap

      // Create new environment and eval the expression in it
      val newEnv = env.extend(bindings)

      evalSeq(body, newEnv)
    }
  }

  def eval(expression:AST, env:Environment):AST = {
    expression match {
      case List(WordToken("lambda"), args:List[WordToken], body:AST) =>
        // Function definition
        Lambda(args, body, env)
      case List(WordToken("define"), name:WordToken, expression:Any) =>
        // Constant definition
        env.extend(Map(name -> expression))
      case (procedure :: exprs) =>
        // Procedure evaluation
        val fun:Lambda = eval(procedure, env)
        val args = exprs.map (eval(_, env))
        fun.apply(args)
      case value:Token =>
        env.lookup(value)
    }
  }

  def evalSeq(expressions:AST, env:Environment):Token = {
    expressions.map(eval(_, env)).last
  }
}
