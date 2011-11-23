package unit

import org.scalatest.Spec
import lisp.Interpreter
import lisp.Parser.AST
import lisp.Lambda
import lisp.Environment
import lisp.RootEnvironment
import lisp.tokens._
import lisp.UndefinedException
import lisp.Pair
import lisp.Lexer
import lisp.Parser

class InterpreterTest extends Spec {
  it("should interpret self-evaluating tokens") {
    val interpreter = new Interpreter

    expect(TrueToken()) { interpreter.eval(TrueToken()) }
    expect(FalseToken()) { interpreter.eval(FalseToken()) }
  }

  it("should interpret lambda definitions") {
    val fun = (new Interpreter).eval(List(WordToken("lambda"), List(WordToken("x")), List(WordToken("x")))).asInstanceOf[Lambda]

    expect(List(WordToken("x"))) { fun.args }
    expect(List(WordToken("x"))) { fun.body }
  }

  it("should apply") {
    val interpreter = new Interpreter
    val fun = interpreter.eval(List(WordToken("lambda"), List(WordToken("x")), List(WordToken("x")))).asInstanceOf[Lambda]

    expect(1) { fun.apply(List(1), interpreter).asInstanceOf[Int] }
  }

  it("should evaluate applications") {
    expect(DecimalToken(1)) { (new Interpreter).eval(List(List(WordToken("lambda"), List(WordToken("x")), List(WordToken("x"))), DecimalToken(1))) }
  }

  it("should return last item in begin definition") {
    expect(DecimalToken(1)) { (new Interpreter).eval(List(WordToken("begin"), DecimalToken(3), DecimalToken(2), DecimalToken(1))) }
  }

  it("should define constants") {
    val interpreter = new Interpreter

    expect(SymbolToken("ok")) { interpreter.eval(List(WordToken("define"), WordToken("a"), DecimalToken(1))) }
    expect(DecimalToken(1)) { RootEnvironment.bindings(WordToken("a")) }
  }

  it("should look up constants") {
    val interpreter = new Interpreter
    interpreter.eval(List(WordToken("define"), WordToken("a"), DecimalToken(1)))
    expect(DecimalToken(1)) { interpreter.eval(WordToken("a")) }
  }

  it("should throw an exception on accessing undefined constants") {
    intercept[UndefinedException] {
      (new Interpreter).eval(List(WordToken("abc")))
    }
  }

  it("should define functions with a simpler syntax") {
    val interpreter = new Interpreter
    interpreter.eval(List(WordToken("define"), List(WordToken("id"), WordToken("x")), WordToken("x")))
    expect(DecimalToken(1.0)) { interpreter.eval(List(WordToken("id"), DecimalToken(1.0))) }
  }

  it("should add") {
    expect(DecimalToken(2)) { (new Interpreter).eval(List(WordToken("+"), DecimalToken(1), DecimalToken(1))) }
  }

  it("should subtract") {
    expect(DecimalToken(1)) { (new Interpreter).eval(List(WordToken("-"), DecimalToken(2), DecimalToken(1))) }
  }

  it("should multiply") {
    expect(DecimalToken(4)) { (new Interpreter).eval(List(WordToken("*"), DecimalToken(2), DecimalToken(2))) }
  }

  it("should divide") {
    expect(DecimalToken(2)) { (new Interpreter).eval(List(WordToken("/"), DecimalToken(4), DecimalToken(2))) }
  }

  it("should cons") {
    expect(Pair(DecimalToken(1), DecimalToken(2))) { (new Interpreter).eval(List(WordToken("cons"), DecimalToken(1), DecimalToken(2))) }
  }

  it("should throw an error on trying to cons more than two values") {
    intercept[Exception] {
      (new Interpreter).eval(List(WordToken("cons"), DecimalToken(1), DecimalToken(1), DecimalToken(1)))
    }
  }

  it("should car and cdr") {
    val interpreter = new Interpreter
    expect(TrueToken()) { interpreter.eval(List(WordToken("car"), List(WordToken("cons"), TrueToken(), FalseToken()))) }
    expect(FalseToken()) { interpreter.eval(List(WordToken("cdr"), List(WordToken("cons"), TrueToken(), FalseToken()))) }
  }

  it("should eq?") {
    val interpreter = new Interpreter

    expect(TrueToken()) { interpreter.eval(List(WordToken("eq?"), TrueToken(), TrueToken()))}
    expect(TrueToken()) { interpreter.eval(List(WordToken("eq?"), DecimalToken(1.0), DecimalToken(1.0)))}

    expect(FalseToken()) { interpreter.eval(List(WordToken("eq?"), TrueToken(), FalseToken()))}
    expect(FalseToken()) { interpreter.eval(List(WordToken("eq?"), DecimalToken(1.0), DecimalToken(1.1)))}
  }

  it("should if") {
    val interpreter = new Interpreter
    expect(SymbolToken("true")) { interpreter.eval(List(WordToken("if"), TrueToken(), SymbolToken("true"), SymbolToken("false")))}
    expect(SymbolToken("false")) { interpreter.eval(List(WordToken("if"), FalseToken(), SymbolToken("true"), SymbolToken("false")))}
  }

  it("should cond") {
    val interpreter = new Interpreter
    expect(SymbolToken("true")) { interpreter.eval(List(WordToken("cond"),
                                                      List(TrueToken(), SymbolToken("true")),
                                                      List(FalseToken(), SymbolToken("false")))) }
    expect(SymbolToken("false")) { interpreter.eval(List(WordToken("cond"),
                                                      List(FalseToken(), SymbolToken("true")),
                                                      List(WordToken("else"), SymbolToken("false")))) }
  }

  it("should evaluate factorials") {
    val lexer = new Lexer
    val parser = new Parser
    val interpreter = new Interpreter
    interpreter.evalSeq(parser.parse(lexer.lex("(define (fact n) (cond ((eq? n 0) 1) (else (* n (fact (- n 1))))))")))
    expect(DecimalToken(3628800)) { interpreter.evalSeq(parser.parse(lexer.lex("(fact 10)"))) }
  }
}
