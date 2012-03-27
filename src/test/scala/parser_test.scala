package unit

import org.scalatest.Spec
import lisp.{Parser, Compiler}
import lisp.ast._

class ParserTest extends Spec {

  val compiler = new Compiler

  it("should parse a simple expression") {
    val code = "(+ 1 2)"
    val ast = App(Val("+"), List(Int(1), Int(2)))
	assertAst(code, ast)
  }

  it("should parse a somewhat more complex expression") {
	val code = "(+ (* 1 (- 4 2)) (/ 4 2))"
    val ast = App(
        Val("+"),
        List(
		  App(
			Val("*"),
			List(
			  Int(1),
			  App(
				Val("-"),
				List(
				  Int(4),
				  Int(2)
				)
			  )
			)
		  ),
          App(
			Val("/"),
			List(
			  Int(4),
			  Int(2)
			)
          )
		)
      )
	assertAst(code, ast)
  }

  it("should parse an abstraction") {
	val code = "(lambda (x) x)"
	val ast = Abs(List(Val("x")), Val("x"))
	assertAst(code, ast)
  }

  it("should parse a multi-character value") {
	val code = "test"
	val ast = Val("test")
	assertAst(code, ast)
  }

  it("should parse evaluate a two-argument function application") {
	val code = "((lambda (x y) (+ x y)) 1)"
	val ast = App(Abs(List(Val("x"), Val("y")), App(Val("+"), List(Val("x"), Val("y")))), List(Int(1)))
	assertAst(code, ast)
  }

  it("should parse a no-argument function application") {
	val code = "(run)"
	val ast = App(Val("run"), List())
	assertAst(code, ast)
  }

  it("should parse a no-argument abstraction") {
	val code = "(lambda () n)"
	val ast = Abs(List(), Val("n"))
	assertAst(code, ast)
  }

  it("should parse booleans") {
	assertAst("#t", True())
	assertAst("#f", False())
  }

  it("should parse nulls") {
	assertAst("null", Null())
  }

  it("should parse if statements") {
	assertAst("(if #t 1)", If(True(), Int(1), Null()))
	assertAst("(if #f 0 1)", If(False(), Int(0), Int(1)))
  }

  protected def assertAst(code:String, ast:Expr) = expect(ast) { compiler.parse(code) }
}
