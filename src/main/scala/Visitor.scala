package org.ball.mini
import ast.*
import ast.Symbol as Sym
import scala.collection.mutable

class Visitor:

  private var scope = SymbolTable(null)
  private var program: String = ""

  def dumpGlobal: String = scope.toString

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
        scope.define(Sym(name, SymType.Var, typeVal))

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

      case Assign(name, expr) =>
        val exprValue = visit(expr)
        val castValue = exprValue match
          case _: Int =>
            BuiltinAstValueType.IntType
          case _: Float =>
            BuiltinAstValueType.Real
          case _: String =>
            BuiltinAstValueType.StringType
          case _ =>
            throw new RuntimeException(s"error value $exprValue")
        scope.setValue(name, exprValue, castValue)
      case NoOp() =>

      case Procedure(name, params, body) =>
        scope.createChildTable(name)
        scope = scope.child(name)
        for param <- params do
          scope.define(param.varname, param.vartype, param.defaultValue)
        body.foreach(visit)
        scope = scope.parent

      case Block(decls, comp) =>
        decls.foreach(varDecl => scope.define(varDecl.name, varDecl.typeVal, null))
        visit(comp)

      case VarCall(name) =>
        val ret = scope.lookup(name)
        ret.retType match
          case BuiltinAstValueType.IntType =>
            ret.getValue.asInstanceOf[Int]
          case BuiltinAstValueType.Real =>
            ret.getValue.asInstanceOf[Float]
          case BuiltinAstValueType.StringType =>
            ret.getValue.asInstanceOf[String]
          case _ =>
            throw new RuntimeException(s"type error, var type: ${ret.retType}")
    end match
  end visit
end Visitor






