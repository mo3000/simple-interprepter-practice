package org.ball.mini
package ast
import java.nio.ByteBuffer


class Symbol(val name: String, val symType: SymType, val retType: AstValueType):

  private var value: Any = null

  private var valueType: AstValueType = null
  

  def setValue(v: Any, t: AstValueType): Unit =
    if valueType == null then
      valueType = t
    else if valueType != t then
      throw new RuntimeException(s"type mismatch. expected $valueType, but encounterd $t")
    value = v
  end setValue
  
  def setValue(v: Int): Unit =
    setValue(v, BuiltinAstValueType.IntType)
    
  def setValue(v: Float): Unit =
    setValue(v, BuiltinAstValueType.Real)
    
  def getValue =
    if value == null then
      throw new RuntimeException("value not set")
    else if symType == SymType.Var then
      value
    else
      throw new RuntimeException("not implemented")

  override def toString: String = s"Sym($value)"
end Symbol


enum SymType:
  case Var
  case Func
  case Method
  case Class
