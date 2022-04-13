package org.ball.mini

import scala.collection.mutable
import org.ball.mini.{Token, Value, Op}

class Interpreter {

  private var text = ""
  private var pos = 0

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
          pos += 1
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
        pos += 1
        v match {
          case '+' => Op.Plus
          case '-' => Op.Minus
          case '*' => Op.Mul
          case '/' => Op.Div
        }
      case v =>
        throw new RuntimeException(s"not implemented: ($v)")
    }


  def tokenList(): Array[Token] =
    val arr = mutable.ArrayBuffer[Token]()
    while !isEof do
      arr.addOne(get_next_token())
    arr.toArray

  def expr(xs: Array[Token]): Int =
    var pos = 0
    var v = factor(xs(0)).value
    pos += 1
    while pos < xs.length && Set(Op.Div, Op.Mul).contains(xs(pos)) do
      val t = xs(pos)
      pos += 1
      t match
        case Op.Div =>
          v /= factor(xs(pos)).value
        case Op.Mul =>
          v *= factor(xs(pos)).value
        case _ =>
          throw RuntimeException("not implemented in expr")

      pos += 1
    v

  def eat[T <: Token](e: Token): T =
    val trans = e.asInstanceOf[T]
    trans

  def factor(t: Token): Value.Integer =
    eat[Value.Integer](t)


  def eval(): AnyVal =
    val parsedExpr = tokenList()
//    println(parsedExpr.mkString(","))
    expr(parsedExpr)
}