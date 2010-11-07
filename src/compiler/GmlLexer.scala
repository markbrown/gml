// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import util.parsing.combinator.lexical.Lexical
import util.parsing.syntax.StdTokens

class GmlLexer extends Lexical with StdTokens {
  import util.parsing.input.CharArrayReader.EofCh
  def whitespace: Parser[Any] =
    rep(whitespaceChar | '%' ~ rep(chrExcept('\n', EofCh)))

  def token: Parser[Token] =
    ( name
    | graphic
    | numLiteral
    | stringLiteral
    )

  def name: Parser[Token] =
    letter ~ rep( letter | digit | '-' | '_' ) ^^ {
      case first ~ rest => Identifier(first::rest mkString "")
    }

  def graphic: Parser[Token] =
    ( ('/': Parser[Char]) | '[' | ']' | '{' | '}' ) ^^
            { c => Keyword(c toString) }

  def stringLiteral: Parser[Token] =
    ( '\"' ~> rep(chrExcept('\"', EofCh)) <~ '\"' ^^
            { cs => StringLit(cs mkString "") }
    | '\"' ~> failure("unclosed string literal")
    )

  def numLiteral: Parser[Token] =
    intLiteral ~ optFrac ~ optExp ^^ {
      case i ~ f ~ e => NumericLit(i + f + e)
    }

  def intLiteral: Parser[String] =
    opt('-') ~ rep1(digit) ^^ {
      case None ~ digits => digits mkString ""
      case Some(_) ~ digits => '-'::digits mkString ""
    }

  def optFrac: Parser[String] =
    opt('.' ~> rep1(digit)) ^^ {
      case None => ""
      case Some(frac) => '.'::frac mkString ""
    }

  def optExp: Parser[String] =
    opt((('e': Parser[Char]) | 'E') ~> intLiteral) ^^ {
      case None => ""
      case Some(exp) => "e" + exp
    }
}
