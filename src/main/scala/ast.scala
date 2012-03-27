package lisp.ast

import com.codecommit.gll.ast._

sealed trait Expr extends Node

case class App(fun:Expr, args:List[Expr]) extends Expr {
  val label = 'app
  val children = fun :: args

  override def toString = "(" + fun.toString + " " + args.map(_.toString).mkString(" ") + ")"
}

case class Abs(args:List[Val], body:Expr) extends Expr {
  val label = 'abs
  val children = body :: args

  override def toString = "(lambda (" + args.map(_.toString).mkString(" ") + ") " + body.toString + ")"
}

case class Int(num:Integer) extends Expr {
  val label = 'int
  val children = Nil

  override def toString = num.toString
}

case class Val(name:String) extends Expr {
  val label = 'val
  val children = Nil

  override def toString = name
}

case class Progn(exprs:List[Expr]) extends Expr {
  val label = 'progn
  val children = exprs
  override def toString = "(progn " + exprs.map(_.toString).mkString(" ") + ")"
}

trait Boolean extends Expr

case class True extends Boolean {
  val label = 'true
  val children = Nil
  
  override def toString = "#t"
}

case class False extends Boolean {
  val label = 'false
  val children = Nil
  
  override def toString = "#f"
}

case class Null extends Expr {
  val label = 'null
  val children = Nil
  
  override def toString = "null"
}

case class If(pred:Expr, cons:Expr, alt:Expr=Null()) extends Expr {
  val label = 'if
  val children = pred :: cons :: alt :: Nil

  override def toString = "(if " + pred.toString + " " + cons.toString + " " + alt.toString + ")"
}
