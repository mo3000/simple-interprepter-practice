package org.ball.mini
package ast


type VarValueType = Int | Float | String


abstract class VarValue():

  private var v: Any = 0

  def value = v

  private var valueType: ValueType = ValueType.Integer

  def numericValue = valueType match
    case ValueType.Real =>
      v.asInstanceOf[Float]
    case ValueType.Integer =>
      v.asInstanceOf[Int]
    case _ =>
      throw new RuntimeException(s"type error: $v is not numeric")

  def setValue(x: Any, vtype: ValueType): Unit =
    v = x
    valueType = vtype

  def setValue(x: Any): Unit = x match
    case _: Int =>
      setValue(x, ValueType.Integer)
    case _: Float =>
      setValue(x, ValueType.Real)
    case _: String =>
      setValue(x, ValueType.String)
    case _ =>
      throw new RuntimeException(s"type error $x")

  def +(y: VarValue): VarValue

  def -(y: VarValue): VarValue


