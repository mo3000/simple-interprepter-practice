package org.ball.mini

import org.ball.mini.ast.*

import scala.collection.mutable
import org.ball.mini.{Op, Token, Value}

class Interpreter {

  private var text = ""
  private var pos = 0
  private var parsedToken: Array[Token] = Array.empty

  def input(text: String): Unit = {
    this.text = text
    pos = 0
  }

  private def isEof: Boolean = pos >= text.length

  private def current = text(pos)

  private def advance(): Unit = pos += 1

  private def skipWhiteSpace(): Unit =
    while !isEof && current.isWhitespace do
      advance()

  private def currentToken: Token = parsedToken(pos)

  private def peek_next: Char =
    if pos + 1 < text.length
      then text(pos + 1)
    else
      0


  private def get_next_token(): Token =
    skipWhiteSpace()
    val start = pos
    current match {
      case v if v.isDigit || ((v == '+' || v == '-') && peek_next.isDigit) =>
        var isFloat = false
        while !isEof && (current.isDigit || current == '.') do
          advance()
          if v == '.' then
            if isFloat then
              throw new RuntimeException("encounter second dot")
            else
              isFloat = true
        val str = text.substring(start, pos)
        if !isFloat then
          Value.Integer(str.toInt)
        else
          Value.FloatNum(str.toFloat)
      case v if Set('+', '-', '*', '/').contains(v) =>
        advance()
        v match {
          case '+' => Op.Plus
          case '-' => Op.Minus
          case '*' => Op.Mul
          case '/' => Op.Div
        }
      case '(' =>
        advance()
        Op.LParen
      case ')' =>
        advance()
        Op.RParen
      case v =>
        throw new RuntimeException(s"not implemented: ($v)")
    }


  def tokenList(): Array[Token] =
    val arr = mutable.ArrayBuffer[Token]()
    while !isEof do
      arr.addOne(get_next_token())
    arr.toArray

  def expr(): AstNode =
    var v = term()
    while pos < parsedToken.length && Set(Op.Plus, Op.Minus).contains(currentToken) do
      val t = currentToken
      advance()
      t match
        case Op.Plus =>
          v = BinOp(Op.Plus, v, term())
        case Op.Minus =>
          v = BinOp(Op.Minus, v, term())
    end while
    v


  def cast[T <: Token](e: Token): T =
    val trans = e.asInstanceOf[T]
    trans

  def eat[T <: Token](e: Token): T =
    val t = cast[T](e)
    pos += 1
    t

  def term(): AstNode =
    var v = factor()
    while pos < parsedToken.length && Set(Op.Mul, Op.Div).contains(currentToken) do
      val t = currentToken
      pos += 1
      t match
        case Op.Mul =>
          v = BinOp(Op.Mul, v, factor())
        case Op.Div =>
          v = BinOp(Op.Div, v, factor())
    end while
    v


  def factor(): AstNode =
    if currentToken == Op.LParen then
      advance()
      val v = expr()
      assert(currentToken == Op.RParen)
      advance()
      v
    else
      Num(eat[Value.Integer](currentToken))


  def astTree(): AstNode =
    val parsedExpr = tokenList()
    parsedToken = parsedExpr
    pos = 0
    expr()
}