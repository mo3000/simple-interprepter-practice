package org.ball.mini
import ast.*

class Visitor:

  def visit(node: AstNode): Int | Float | String =
    node match
      case BinOp(op, left, right) =>
        val l = visit(left).asInstanceOf[Int]
        val r = visit(right).asInstanceOf[Int]
        op match
          case Op.Minus =>
            l - r
          case Op.Plus =>
            l + r
          case Op.Div =>
            l / r
          case Op.Mul =>
            l * r
        end match
      case Num(v) =>
        v.value match {
          case i: Int => i
          case fl: Float => fl
          case v => throw RuntimeException(s"unexpected token $v")
        }
      case UnaryOp(op, node) =>
        val v = visit(node)
        assert(v.isInstanceOf[Int] || v.isInstanceOf[Float])
        if op == Op.Minus then
          v match {
            case i: Int => -i
            case fl: Float => -fl
            case _ => throw RuntimeException(s"type error: $v")
          }
        else
          v
    end match
  end visit
end Visitor






