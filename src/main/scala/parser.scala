package lisp

import lisp.ast._

import com.codecommit.gll._
import com.codecommit.gll.ast._

class Parser extends RegexParsers {

  import Filters._

  lazy val expr: Parser[Expr] = (
	"(" ~ "lambda" ~ "(" ~ valList ~ ")" ~ expr ~ ")" ^^ { (_,_,_,args,_,body,_) => Abs(args, body) }
	| "(" ~ "lambda" ~ "(" ~ ")" ~ expr ~ ")" ^^ { (_,_,_,_,body,_) => Abs(List(), body) }
	| "(" ~ "if" ~ expr ~ expr ~ expr ~ ")" ^^ { (_,_,pred,cons,alt,_) => If(pred,cons,alt) }
	| "(" ~ "if" ~ expr ~ expr ~ ")" ^^ { (_,_,pred,cons,_) => If(pred,cons) }
	| """(-)?\d+""".r ^^ { d => Int(d.toInt) }
	| "(" ~ expr ~ exprList ~ ")" ^^ { (_,fun,arg,_) => App(fun, arg) }
	| "(" ~ expr ~ ")"            ^^ { (_,fun,_) => App(fun, List()) }
	| "null" ^^ { _ => Null() }
	| "#t" ^^ { _ => True() }
	| "#f" ^^ { _ => False() }
	| value
  ) filter ( _ match {
    // Ignore applications of something to lambda, since those are actually abstractions
	case App(Val("lambda"), _) => false
	case App(Val("if"), _) => false
	case Val("#t") => false
	case Val("#f") => false
	case Val("null") => false
	case _ => true
  })

  lazy val exprList: Parser[List[Expr]] = (
	  expr            ^^ { _ :: Nil }
	| expr ~ exprList ^^ { (x,xs) => x :: xs }
  )

  lazy val valList: Parser[List[Val]] = (
	  value           ^^ { _ :: Nil }
	| value ~ valList ^^ { (x,xs) => x :: xs }
  )
  
  val startRegex = "A-Za-z\\!\\@\\#\\$\\%\\^\\&\\*\\-\\=\\_\\+\\<\\>\\.\\?\\/"
  val restRegex = "0-9"
  lazy val value: Parser[Val] = {
	("[" + startRegex + "][" + restRegex + startRegex + "]*").r ^^ { Val(_) }
  }

  def parser = expr

}
