package lisp

import lisp.ast._

import com.codecommit.gll._
import scala.collection.GenTraversableOnce
import java.util.UUID

class Environment(parent:Environment=RootEnvironment, bindings:Map[Val,Expr]=Map()) {
  def extend(newBindings:GenTraversableOnce[(Val,Expr)]):Environment = {
	new Environment(this, bindings ++ newBindings)
  }

  def lookup(value:Val):Expr = bindings.get(value) match {
	case Some(result) => result
	case None => parent.lookup(value)
  }
}

object RootEnvironment extends Environment {
  val parent = null
  val bindings = Map[Val,Expr]()

  override def lookup(value:Val):Expr = bindings.getOrElse(value, value)
}

class Compiler {
  val parser = new Parser

  def compile(expr:Expr):Expr = {
	hoist(toCPS(curry(desugar(expr)), Val("halt")))
  }

  def parse(code:String):Expr = {
	val Success(result, _) = parser.parser(code).head
	result
  }

  def hoist(expr:Expr):Expr = {
	val (result, funs) = hoistToDefs(expr)
	val defs = funs.map(pair => App(Val("define"), List(pair._1, pair._2)))
	Progn(result :: defs )
  }

  def hoistToDefs(expr:Expr, defs:List[(Val,Expr)]=Nil):(Expr, List[(Val, Expr)]) = {
	expr match {
	  case Abs(args, body) =>
		val (newBody, bodyDefs) = hoistToDefs(body)
		val name = genUniqueVal("fun")
	    val funId = (name, Abs(args, newBody))
	    (name, (funId :: defs) ::: bodyDefs)

	  case App(fun, args) =>
		val (hoistedFun, funDefs) = hoistToDefs(fun)
	    val (hoistedArgs, argDefs) = args.map(hoistToDefs(_)).unzip
	    (App(hoistedFun, hoistedArgs), funDefs ::: argDefs.flatten ::: defs)

	  case If(pred, cons, alt) =>
		val (hoistedPred, predDefs) = hoistToDefs(pred)
		val (hoistedCons, consDefs) = hoistToDefs(cons)
	    val (hoistedAlt, altDefs) = hoistToDefs(alt)
	    (If(hoistedPred, hoistedCons, hoistedAlt), predDefs ::: consDefs ::: altDefs ::: defs)

	  case _ => (expr, defs)
	}
  }

  def desugar(expr:Expr):Expr = {
	expr match {
	  case App(Val("let"), List(App(kv, kvs), body)) =>
		val exprs = kv :: kvs
		val (names, values) = exprs.map( expr => expr match {
		  case App(Val(name), List(value)) => (Val(name), value)
		  case _ => throw new Exception("Syntax error: let")
		}).unzip
		App(Abs(names, body), values)

	  case App(Val("cond"), List(App(e, List()))) =>
		val App(pred, List(cons)) = e
	    If(pred, cons)
	  case App(Val("cond"), List(App(e, es))) =>
		val App(pred, List(cons)) = e
		If(pred, cons, desugar(App(Val("cond"), es)))

	  case _ => expr
	}
  }

  def curry(expr:Expr):Expr = {
	expr match {
	  case App(fun, arg :: Nil) =>
		App(fun, List(arg))
	  case App(fun, arg :: args) =>
		curry(App(App(curry(fun), List(arg)), args))
	  case Abs(arg :: Nil, body) =>
		Abs(List(arg), curry(body))
	  case Abs(arg :: args, body) =>
		Abs(List(arg), curry(Abs(args, body)))
	  case If(pred, cons, alt) =>
		If(curry(pred), curry(cons), curry(alt))
	  case _ => expr
	}
  }

  def atomicCPS(expr:Expr):Expr = {
	expr match {
	  case Abs(List(arg), body) =>
		val cont = genUniqueVal("k")
		Abs(List(arg, cont), toCPS(body, cont))
	  case _ => expr
	}
  }

  def toCPS(expr:Expr, cont:Expr):Expr = {
	expr match {
	  case App(fun, List(arg)) =>
		val f = genUniqueVal("f")
		val e = genUniqueVal("e")
		toCPS(fun, Abs(List(f),
					 toCPS(arg, Abs(List(e),
								  App(f, List(e, cont))))))
	  case If(pred, cons, alt) =>
		val k = genUniqueVal("k")
		Abs(List(k),
			toCPS(pred, Abs(List(Val("aexp")),
							If(Val("aexp"), toCPS(cons, k), toCPS(alt, k)))))
	  case _ =>
		App(cont, List(atomicCPS(expr)))

	}
  }

  protected def genUniqueVal(name:String):Val = {
	Val(name + "-" + UUID.randomUUID().toString)
  }
}
