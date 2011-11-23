package lisp.tokens

class Token
class IgnoreToken extends Token
case class HaltingToken() extends Token

class BooleanToken extends Token

case class TrueToken() extends BooleanToken {
  override def toString() = "#t"
}
case class FalseToken() extends BooleanToken {
  override def toString() = "#f"
}

case class NullToken() extends Token {
  override def toString() = "null"
}

case class WordToken(val word:String) extends Token {
  override def toString() = word
}

case class ParenToken(val which:Symbol) extends Token {
  override def toString() = which match {
    case 'open => "("
    case 'close => ")"
  }
}

case class DecimalToken(val value:Double) extends Token {
  override def toString() = value.toString()
}

case class WhitespaceToken() extends IgnoreToken

case class SymbolToken(val value:String) extends Token {
  override def toString() = "'" + value
}

case class CharToken(val char:Char) extends Token {
  override def toString() = char.toString()
}
case class StringToken(val string:String) extends Token {
  override def toString() = '"' + string + '"'
}
