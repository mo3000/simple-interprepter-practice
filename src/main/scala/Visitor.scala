package org.ball.mini
import ast.*
import scala.collection.mutable

class Visitor:

  private val globalScope = mutable.HashMap[String, Int | Float | String]()
  private var program: String = ""

  def dumpGlobal: String = globalScope.toString

  type VisitorOutput = Unit | Int | Float | String

  def numericCastInt(x: VisitorOutput): Int =
    if x.isInstanceOf[Int] then
      x.asInstanceOf[Int]
    else
      x.asInstanceOf[Float].toInt

  def numericCastFloat(x: VisitorOutput): Float =
    if x.isInstanceOf[Int] then
      x.asInstanceOf[Int].toFloat
    else
      x.asInstanceOf[Float]



  def visit(node: AstNode): VisitorOutput =
    node match
      case BinOp(op, left, right) =>
        val l = visit(left)
        val r = visit(right)
        val testFloat = l.isInstanceOf[Float] || r.isInstanceOf[Float]
        op match
          case Op.Minus =>
            if testFloat then
              numericCastFloat(l) - numericCastFloat(r)
            else
              numericCastInt(l) - numericCastInt(r)
          case Op.IntDiv =>
            val rfloor = numericCastInt(r)
            assert(r != 0)
            numericCastInt(l) / rfloor
          case Op.Plus =>
            if testFloat then
              numericCastFloat(l) + numericCastFloat(r)
            else
              numericCastInt(l) + numericCastInt(r)
          case Op.Div =>
            assert(r != 0)
            if testFloat then
              numericCastFloat(l) / numericCastFloat(r)
            else
              numericCastInt(l) / numericCastInt(r)
          case Op.Mul =>
            if testFloat then
              numericCastFloat(l) * numericCastFloat(r)
            else
              numericCastInt(l) * numericCastInt(r)
        end match
      case Num(v) =>
        v.value match {
          case i: Int => i
          case fl: Float => fl
          case v => throw RuntimeException(s"unexpected token $v")
        }
      case Program(name, Block(decls, comp)) =>
        program = name
        if decls.nonEmpty then
          decls.foreach(visit)
        visit(comp)
      case VarDecl(name, typeVal) =>
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
        globalScope(v) = exprValue match
          case _: Int =>
            exprValue.asInstanceOf[Int]
          case _: Float =>
            exprValue.asInstanceOf[Float]
          case _: String =>
            exprValue.asInstanceOf[String]
          case _ =>
            throw new RuntimeException(s"error value $exprValue")
      case NoOp() =>
      case VarCall(name) =>
        globalScope(name)
    end match
  end visit
end Visitor






