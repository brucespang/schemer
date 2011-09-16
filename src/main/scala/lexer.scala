package lisp

import lisp.tokens._
import scala.util.matching.Regex

class Lexer extends AbstractLexer {
  class StringLexer(val caller:Lexer, var string:String="") extends AbstractLexer{
    override def handleLexFinish(text:String, tokens:List[Token]):(List[Token],String) = {
      val charTokens = tokens.reverse
      val stringToken = StringToken(charTokens.map { token =>
        token match {
          case t: CharToken => t.char
          case _ => null
        }
      }.filter(_.isInstanceOf[Char]).mkString)

      (List(stringToken), text)
    }

    // match end of string
    pattern("\"".r) { _ => HaltingToken() }

    // match escaped slashes
    val escapeSequence = "\\\\"

    pattern((escapeSequence + "\\\\").r) { _ => CharToken('\\') }
    pattern((escapeSequence + "\"").r) { _ => CharToken('"') }

    // match any character
    pattern(".".r) { c => CharToken(c.head) }
  }

  class CommentLexer(val caller:Lexer) extends AbstractLexer {
    pattern("[^$]".r) { _ => new IgnoreToken }
    pattern("$".r) { _ => caller }
  }

  pattern("\\(".r) ( _ => ParenToken('open) )
  pattern("\\)".r) ( _ => ParenToken('close) )
  pattern("\"".r) ( _ => new StringLexer(this) )
  pattern(";".r) ( _ => new CommentLexer(this) )
  pattern("\\#t".r) ( _ => TrueToken() )
  pattern("\\#f".r) ( _ => FalseToken() )
  pattern("[0-9]+\\.[0-9]+".r) ( num => DecimalToken(num.toDouble) )
  pattern("[0-9]+".r) ( num => IntToken(num.toInt) )
  pattern("[(\\+|\\-|\\*|\\/|\\*\\*|\\.|\\<|\\=|\\>|\\<\\=|\\>\\=)]".r) ( punct => PunctuationToken(punct))
  pattern("[0-9A-Za-z!$%&*+-./:<=>?@^_~]+".r) ( word => WordToken(word))
  pattern("\\s+".r) { _ => WhitespaceToken()}
}
