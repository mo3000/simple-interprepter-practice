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

  private def get_next_token(): Token =
    while !isEof && current.isWhitespace do
      pos += 1
    val start = pos
    current match {
      case v if v.isDigit || ((v == '+' || v == '-') && pos + 1 < text.length && text(pos + 1).isDigit) =>
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

  def expr(): Array[Token] =
    val arr = mutable.ArrayBuffer[Token]()
    while !isEof do
      arr.addOne(get_next_token())
    arr.toArray

  def eval(): AnyVal =
    val parsedExpr = expr()
    val left = parsedExpr(0)
    val op = parsedExpr(1)
    val right = parsedExpr(2)
    if !op.isOp then throw RuntimeException("is not op")
    op match {
      case Op.Plus =>
        left.asInstanceOf[Value.Integer].value + right.asInstanceOf[Value.Integer].value
      case Op.Minus =>
        left.asInstanceOf[Value.Integer].value - right.asInstanceOf[Value.Integer].value
      case Op.Mul =>
        left.asInstanceOf[Value.Integer].value * right.asInstanceOf[Value.Integer].value
      case _ =>
        throw RuntimeException(s"not implemented op: $op")
    }

}