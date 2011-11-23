package unit

import org.scalatest.Spec
import lisp.tokens._
import lisp.Lexer

class LexerTest extends Spec {
  it("should lex each token properly") {
    val lexer = new Lexer

    var text = "("
    expect(List(ParenToken('open))) { lexer.lex(text) }

    text = ")"
    expect(List(ParenToken('close))) { lexer.lex(text) }

    text = "#t"
    expect(List(TrueToken())) { lexer.lex(text) }

    text = "#f"
    expect(List(FalseToken())) { lexer.lex(text) }

    text = "1"
    expect(List(DecimalToken(1))) { lexer.lex(text) }

    text = "1.0"
    expect(List(DecimalToken(1.0))) { lexer.lex(text) }

    text = "let*!"
    expect(List(WordToken(text))) { lexer.lex(text) }

    text = "*"
    expect(List(WordToken(text))) { lexer.lex(text) }

    text = "null"
    expect(List(NullToken())) { lexer.lex(text) }
  }

  it("should lex a series of tokens properly") {
    val text = "(+ 1 2)"

    expect(List(ParenToken('open), WordToken("+"), DecimalToken(1), DecimalToken(2), ParenToken('close))) { (new Lexer).lex(text) }
  }

  it("should ignore comments") {
    expect(List()) { (new Lexer).lex(";; This is a comment") }
  }

  it("should lex strings") {
    expect(List(StringToken("abc"))) { (new Lexer).lex("\"abc\"") }
  }

  it("should lex quotes in strings") {
    expect(List(StringToken("\""))) { (new Lexer).lex("\"\\\"\"")}
  }

  it("should lex slashes in strings") {
    expect(List(StringToken("\\"))) { (new Lexer).lex("\"\\\\\"")}
  }

  it("should lex a symbol") {
    expect(List(SymbolToken("abc"))) { (new Lexer).lex("'abc") }
  }
}
