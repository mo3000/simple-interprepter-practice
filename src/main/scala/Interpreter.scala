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


  def expr(): Array[Token] =
    val arr = mutable.ArrayBuffer[Token]()
    while !isEof do
      arr.addOne(get_next_token())
    arr.toArray

  def eat[T <: Token](e: Token): T =
    val trans = e.asInstanceOf[T]
    trans


  def eval(): AnyVal =
    val parsedExpr = expr()
//    println(parsedExpr.mkString(","))
    var carry = eat[Value.Integer](parsedExpr(0)).value
    var i = 1
    while i < parsedExpr.length do
      val op = eat[Op](parsedExpr(i))
      i += 1
      if !op.isOp then throw RuntimeException("is not op")
      val right = eat[Value.Integer](parsedExpr(i))
      carry = op match {
        case Op.Plus =>
          carry + right.value
        case Op.Minus =>
          carry - right.value
        case Op.Mul =>
          carry * right.value
        case Op.Div =>
          if right.value == 0 then throw RuntimeException("div by 0")
          carry / right.value
        case _ =>
          throw RuntimeException(s"not implemented op: $op")
      }
      i += 1
    carry
}