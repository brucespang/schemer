package lisp

import lisp.tokens._
import scala.util.matching.Regex
import scala.collection.mutable.ListMap

trait AbstractLexer {
  implicit def Token2Right(t:Token) = Right(t)
  implicit def AbstractLexer2Left(l:AbstractLexer) = Left(l)

  // If the result of the handler is a lexer, we start lexing from the rest of the string,
  // and append the result of the sublexer to the list of tokens
  // Otherwise, we append the token to the list of tokens.
  type Handler = (String => Either[AbstractLexer, Token])

  // ListMap to preserve the order of the patterns for predictable overriding behavior
  protected var patterns = ListMap[Regex,Handler]()

  def pattern(pattern:Regex) = {
    { handler:Handler =>
      val newPattern = ("^(" + pattern + ")").r
      patterns.update(newPattern, handler)
    }
  }

  def lex(text:String):List[Token] = {
    incompleteLex(text)._1
  }

  def incompleteLex(text:String, tokens:List[Token]=List()):Tuple2[List[Token], String] = {
    text match {
      case "" => handleLexFinish(text, tokens)
      case _ =>
        val (pattern, string) = patternWithLongestMatch(text, patterns.keys.toList)

        val restText = text.substring(string.length, text.length)

        patterns(pattern)(string) fold (
          { lexer =>
            val (returnedTokens, returnedText) = lexer.incompleteLex(restText, tokens)
            incompleteLex(returnedText, tokens ::: returnedTokens)
          },
          { token =>
            token match {
              case t: HaltingToken => handleLexFinish(restText, tokens)
              case _ => incompleteLex(restText, token :: tokens)
            }
          }
        )
    }
  }

  protected def handleLexFinish(text:String, tokens:List[Token]):Tuple2[List[Token], String] = {
    (tokens.reverse.filterNot(_.isInstanceOf[IgnoreToken]), text)
  }

  protected def longestSubstring(string:String, pattern:Regex):String = {
    pattern.findAllIn(string).toList.sortBy(_.length) match {
      case List() => ""
      case longest :: rest => longest
    }
  }

  protected def patternWithLongestMatch(string:String, patterns:List[Regex]):(Regex,String) = {
    patterns map (p => (p, longestSubstring(string,p))) filter (_._2.length > 0) match {
      case List() => throw new Exception("No matching pattern for: '" + string + "'")
      case matchingPatterns => matchingPatterns.sortWith(_._2.length > _._2.length).head
    }
  }
}
