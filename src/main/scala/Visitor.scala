package org.ball.mini
import ast.*
import scala.collection.mutable

class Visitor:

  private val globalScope = mutable.HashMap[String, Int | Float | String]()

  def dumpGlobal: String = globalScope.toString


  def visit(node: AstNode): Unit | Int | Float | String =
    node match
      case BinOp(op, left, right) =>
        val l = visit(left)
        val r = visit(right)
        val testFloat = l.isInstanceOf[Float] || r.isInstanceOf[Float]
        op match
          case Op.Minus =>
            if testFloat then
              l.asInstanceOf[Float] - r.asInstanceOf[Float]
            else
              l.asInstanceOf[Int] - r.asInstanceOf[Int]
          case Op.Plus =>
            if testFloat then
              l.asInstanceOf[Float] + r.asInstanceOf[Float]
            else
              l.asInstanceOf[Int] + r.asInstanceOf[Int]
          case Op.Div =>
            assert(r != 0)
            if testFloat then
              l.asInstanceOf[Float] / r.asInstanceOf[Float]
            else
              l.asInstanceOf[Int] / r.asInstanceOf[Int]
          case Op.Mul =>
            if testFloat then
              l.asInstanceOf[Float] * r.asInstanceOf[Float]
            else
              l.asInstanceOf[Int] * r.asInstanceOf[Int]
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
      case Compound(stmts) =>
        stmts.foreach(visit)
      case Assign(v, expr) =>
        val exprValue = visit(expr)
        globalScope(v.name) = exprValue match
          case _: Int =>
            exprValue.asInstanceOf[Int]
          case _: Float =>
            exprValue.asInstanceOf[Float]
          case _: String =>
            exprValue.asInstanceOf[String]
          case _ =>
            throw new RuntimeException(s"error value $exprValue")
      case NoOp() =>
      case Var(name) =>
        globalScope(name)
    end match
  end visit
end Visitor






