package unit

import org.scalatest.Spec
import lisp.Parser
import lisp.tokens._

class ParserTest extends Spec {
  it("should parse a simple expression") {
    val code = List(
      ParenToken('open),
        PunctuationToken("+"),
        IntToken(1),
        IntToken(2),
      ParenToken('close))

    expect(List(List(PunctuationToken("+"), IntToken(1), IntToken(2)))) { (new Parser).parse(code) }
  }

  it("should parse a somewhat more complex expression") {
    val code = List(
      ParenToken('open),
        PunctuationToken("+"),
        ParenToken('open),
          PunctuationToken("*"),
          IntToken(1),
          ParenToken('open),
            PunctuationToken("-"),
            IntToken(4),
            IntToken(2),
          ParenToken('close),
        ParenToken('close),
        ParenToken('open),
          PunctuationToken("/"),
          IntToken(4),
          IntToken(2),
        ParenToken('close),
      ParenToken('close))

    expect(List(
      List(
        PunctuationToken("+"),
        List(
          PunctuationToken("*"),
          IntToken(1),
          List(
            PunctuationToken("-"),
            IntToken(4),
            IntToken(2)
          )
        ),
        List(
          PunctuationToken("/"),
          IntToken(4),
          IntToken(2)
        )
      ))) { (new Parser).parse(code) }
  }
}
