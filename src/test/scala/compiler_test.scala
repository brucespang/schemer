package unit

import org.scalatest._
import matchers.ShouldMatchers._
import Inside._

import lisp.Parser
import lisp.ast._
import com.codecommit.gll._
import lisp.{Compiler, RootEnvironment}

class CompilerTest extends Spec {
  val compiler = new Compiler

  it("should curry") {
	assertCurry("(lambda (x y) (+ x y))",
			    "(lambda (x) (lambda (y) ((+ x) y)))")
	assertCurry("((lambda (x y) (+ x y)) 1 2)",
			    "(((lambda (x) (lambda (y) ((+ x) y))) 1) 2)")
	assertCurry("(if #t (lambda (x y) (+ x y)))",
				"(if #t (lambda (x) (lambda (y) ((+ x) y))))")
  }

  it("should convert to CPS") {
	inside (compiler.toCPS(parse("(g a)"), Val("halt"))) { case App(fun, List(Val("g"))) =>
	  inside (fun) { case Abs(List(f), body) =>
		inside (body) { case App(fun, List(Val("a"))) =>
		  inside (fun) { case Abs(List(e), app) =>
			inside (app) { case App(f, List(e, Val("halt"))) =>
			}
		  }
		}
	  }
	}
  }

  it("should desugar let statements to function application") {
	assertDesugar("(let ((x 1) (y 2)) (+ x y))",
				  "((lambda (x y) (+ x y)) 1 2)")
  }

  it("should desugar conds to ifs") {
	assertDesugar("(cond ((#t 1)))",
				  "(if #t 1)")
  }

  it("should hoist functions") {
	val src = "(lambda (x) x)"
	inside (compiler.hoist(compiler.parse(src))) { case Progn(exprs) =>
	  inside (exprs) { case List(f, defs) =>
		inside (defs) { case App(Val("define"), List(f, Abs(List(Val("x")), Val("x")))) =>
		}
	  }
	}
  }
  
  protected def parse = compiler parse _
  protected def assertCurry(in:String, out:String) = expect(parse(out)) { compiler.curry(parse(in)) }
  protected def assertDesugar(in:String, out:String) = parse(out) should equal (compiler.desugar(parse(in)))
}
