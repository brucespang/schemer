package lisp.tokens

class Token
class IgnoreToken extends Token
case class HaltingToken() extends Token


class BooleanToken extends Token

case class TrueToken() extends BooleanToken
case class FalseToken() extends BooleanToken

case class WordToken(val word:String) extends Token

case class PunctuationToken(val punctuation:String) extends Token

case class ParenToken(val which:Symbol) extends Token

case class IntToken(val int:Int) extends Token
case class DecimalToken(val decimal:Double) extends Token

case class WhitespaceToken() extends IgnoreToken

case class CharToken(val char:Char) extends Token
case class StringToken(val string:String) extends Token
