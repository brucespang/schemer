package lisp

import lisp.tokens._
import lisp.Parser.AST
import collection.mutable.Map

case class Lambda(val args:List[WordToken], val body:AST, val env:Environment) {
  def apply(values:List[Any], Interpreter:Interpreter):Any = {
    if(args.length != values.length)
      throw new Exception

    // Combine each arg with its respective value and convert them to a map
    // Ideally this would all be immutable, but we don't have that yet
    val bindings = collection.mutable.Map() ++ args.zip(values).toMap

    // Create new environment and eval the expression in it
    val newEnv = env.extend(bindings)

    Interpreter.evalSeq(body, newEnv)
  }
}

case class Pair(val car:Any, val cdr:Any) {
  override def toString() = "(" + car + " . " + cdr + ")"
}

case class Environment(parent:Environment=RootEnvironment, val bindings:Map[WordToken, Any] = Map()) {
  def extend(bindings:Map[WordToken, Any]):Environment = new Environment(this, bindings)

  def define(wordToken:WordToken, expression:Any):SymbolToken = {
    bindings(wordToken) = expression
    SymbolToken("ok")
  }

  def lookup(token:Any):Any = {
    token match {
      case token: WordToken =>
        val word = token.asInstanceOf[WordToken]
        if(bindings.contains(word))
          bindings(word)
        else
          handleLookupMiss(token)
      case token: Token => 
        token
    }
  }

  def handleLookupMiss(token:Any):Any = {
    parent.lookup(token)
  }
}

class UndefinedException extends Exception

object RootEnvironment extends Environment(null, Map[WordToken,Any]()) {
  object Lib {
    def add(nums:List[DecimalToken]) = {
      DecimalToken(nums.foldLeft(0.0)((total,i) => total + i.value))
    }

    def subtract(nums:List[DecimalToken]) = {
      DecimalToken(nums.tail.foldLeft(nums.head.value)((total,i) => total - i.value))
    }

    def multiply(nums:List[DecimalToken]) = {
      DecimalToken(nums.foldLeft(1.0)((total,i) => total * i.value))
    }

    def divide(nums:List[DecimalToken]) = {
      DecimalToken(nums.tail.foldLeft(nums.head.value)((total,i) => total / i.value))
    }

    def cons(vals:List[Any]) = vals match {
      case first :: last :: Nil => Pair(first, last)
      case _ => throw new Exception("Too many arguments -- CONS")
    }

    def car(pairs:List[Pair]) = pairs match {
      case pair :: Nil => pair.car
      case _ => throw new Exception("Too many arguments -- CAR")
    }

    def cdr(pairs:List[Pair]) = pairs match {
      case pair :: Nil => pair.cdr
      case _ => throw new Exception("Too many arguments -- CDR")
    }

    def equal(vals:List[Any]):BooleanToken = {
      if(vals.length < 2)
        throw new Exception("Too few arguments -- EQ?")
      vals.tail.foldLeft[Tuple2[BooleanToken,Any]]((TrueToken(), vals.head)) { (current,next) =>
        current._1 match {
          case TrueToken() => (if(current._2 == next) TrueToken() else FalseToken(), next)
          case FalseToken() => current
        }
      }._1
    }
  }

  val libFuns = Map[WordToken, (List[Any] => Any)](
    WordToken("+") -> { decimals => Lib.add(decimals.asInstanceOf[List[DecimalToken]]) },
    WordToken("-") -> { decimals => Lib.subtract(decimals.asInstanceOf[List[DecimalToken]]) },
    WordToken("*") -> { decimals => Lib.multiply(decimals.asInstanceOf[List[DecimalToken]]) },
    WordToken("/") -> { decimals => Lib.divide(decimals.asInstanceOf[List[DecimalToken]]) },
    WordToken("cons") -> Lib.cons,
    WordToken("car") -> { pair => Lib.car(pair.asInstanceOf[List[Pair]]) },
    WordToken("cdr") -> { pair => Lib.cdr(pair.asInstanceOf[List[Pair]]) },
    WordToken("eq?") -> { vals => Lib.equal(vals) },
    WordToken("print") -> { vals => println(vals.mkString(" ")) }
  ) 

  override def handleLookupMiss(token:Any):((List[Any],Interpreter) => Any) = {
    token match {
      case token:WordToken if libFuns.contains(token) =>
        { (vals:List[Any], Interpreter:Interpreter) => libFuns(token)(vals)}
      case _ => throw new UndefinedException
    }
  }
}

class Interpreter {

  def eval(expression:Any, env:Environment=RootEnvironment):Any = {
    expression match {
      case List(WordToken("lambda"), args:List[WordToken], body:AST) =>
        Lambda(args, body, env)
      case List(WordToken("define"), name:WordToken, expression:Any) =>
        env.define(name, eval(expression, env))
      case WordToken("define") :: (args:List[WordToken]) :: (expression:AST) =>
        env.define(args.head, Lambda(args.tail, expression, env))
      case WordToken("begin") :: exps =>
        evalSeq(exps, env)
      case WordToken("if") :: cond :: exprs =>
        if(eval(cond, env) == TrueToken())
          eval(exprs.head, env)
        else
          eval(exprs.last, env)
      case WordToken("cond") :: clauses =>
        eval(expandCond(clauses), env)
      case procedure :: exprs =>
        // Procedure evaluation
        val fun = eval(procedure, env).asInstanceOf[{def apply(args:List[Any], Interpreter:Interpreter):Any}]
        val args = exprs.map (eval(_, env))
        fun.apply(args, this)
      case value =>
        env.lookup(value)
    }
  }

  def evalSeq(expressions:List[Any], env:Environment=RootEnvironment):Any = {
    expressions.map(eval(_, env)).last
  }

  def makeIf(clause:Any, then:Any, consequent:Any) = List(WordToken("if"), clause, then, consequent)

  def seqToExpr(clauses:List[Any]) = WordToken("begin") :: clauses

  def expandCond(clauses:List[Any]):Any = {
    def condPredicate(clause:List[Any]) = clause.head
    def condActions(clause:List[Any]) = clause.tail

    clauses match {
      case List(WordToken("else"), clauses) :: rest if rest.length > 0 => throw new Exception("else clause is not last -- cond")
      case (WordToken("else") :: clauses) :: rest => seqToExpr(clauses)
      case (head:List[Any]) :: tail => makeIf(condPredicate(head), seqToExpr(condActions(head)), expandCond(tail))
      case List() => FalseToken()
    }
  }

}
