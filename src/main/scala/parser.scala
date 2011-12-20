package lisp

import lisp.tokens._

object Parser {
  // This is a super hackish type, but I can't figure out how to express type that is
  // a list of either tokens or itself.
  type AST = List[Any]
}

class MismatchedException extends Exception

class Parser {
  import Parser.AST

  def parse(tokens:List[Token]):AST = {
    _parse(tokens, List())._1.reverse
  }

  protected def _parse(tokens:List[Token], list:AST, level:Int=0):(AST, List[Token]) = {
    tokens match {
      case ParenToken('open) :: tail =>
        val (parsed, rest) = _parse(tail, List(), level + 1)
        _parse(rest, parsed :: list, level)
      case ParenToken('close) :: tail => 
        (list.reverse, tail)
      case token :: tail =>
        _parse(tail, token :: list)
      case List() =>
        level match {
          case 0 => (list, List())
          case _ => throw new MismatchedException
        }
    }
  }
}
