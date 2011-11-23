package unit

import org.scalatest.Spec
import lisp.Parser
import lisp.tokens._
import lisp.MismatchedException

class ParserTest extends Spec {
  it("should parse a simple expression") {
    val code = List(
      ParenToken('open),
        WordToken("+"),
        DecimalToken(1),
        DecimalToken(2),
      ParenToken('close))

    expect(List(List(WordToken("+"), DecimalToken(1), DecimalToken(2)))) { (new Parser).parse(code) }
  }

  it("should parse a somewhat more complex expression") {
    val code = List(
      ParenToken('open),
        WordToken("+"),
        ParenToken('open),
          WordToken("*"),
          DecimalToken(1),
          ParenToken('open),
            WordToken("-"),
            DecimalToken(4),
            DecimalToken(2),
          ParenToken('close),
        ParenToken('close),
        ParenToken('open),
          WordToken("/"),
          DecimalToken(4),
          DecimalToken(2),
        ParenToken('close),
      ParenToken('close))

    expect(List(
      List(
        WordToken("+"),
        List(
          WordToken("*"),
          DecimalToken(1),
          List(
            WordToken("-"),
            DecimalToken(4),
            DecimalToken(2)
          )
        ),
        List(
          WordToken("/"),
          DecimalToken(4),
          DecimalToken(2)
        )
      ))) { (new Parser).parse(code) }
  }

  it("should raise an exception on mismatched parenthesis") {
    intercept[MismatchedException] {
      (new Parser).parse(List(ParenToken('open)))
    }
  }
}
