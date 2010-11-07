// Copyright (C) 2010 Mark Brown.  See the file LICENSE for details.

package compiler

import util.parsing.combinator.syntactical.StdTokenParsers
import util.parsing.syntax.StdTokens

object GmlParser extends StdTokenParsers {
  type Tokens = StdTokens
  val lexical = new GmlLexer

  def parse(s: String): ParseResult[Array[Term]] =
    phrase(terms)(new lexical.Scanner(s))

  def apply(s: String): Array[Term] =
    parse(s) match {
      case Success(terms, _) => terms
      case e: NoSuccess => throw new Exception("syntax error: "+ e.msg)
    }

  def terms: Parser[Array[Term]] =
    rep(term) ^^ { Array.concat(_) }

  def term: Parser[Term] =
    ( ident                 ^^ { processName(_) }
    | "{" ~> terms <~ "}"   ^^ { FunctionTerm(_) }
    | "[" ~> terms <~ "]"   ^^ { ArrayCtor(_) }
    | "/" ~> ident          ^^ { Binder(_) }
    | numericLit            ^^ { processNum(_) }
    | stringLit             ^^ { StringLiteral(_) }
    )

  import runtime.Operators
  private def processName(name: String): Term =
    if (name == "render") Render
    else if (name == "apply") Call(CallType.Apply)
    else if (name == "if") Call(CallType.If)
    else if (Operators isDefinedAt name) Operator(name)
    else Identifier(name)

  private def processNum(num: String): Term =
    if (num matches "-?[0-9]+") IntLiteral(num.toInt)
    else RealLiteral(num.toDouble)
}
