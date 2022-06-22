package org.ball.mini
package ast

import scala.annotation.targetName


type VarValueType = Int | Float | String


abstract class VarValue():

  private var v: Any = 0

  def value = v

  private var builtinValueType: BuiltinAstValueType = BuiltinAstValueType.IntType

  def numericValue = builtinValueType match
    case BuiltinAstValueType.Real =>
      v.asInstanceOf[Float]
    case BuiltinAstValueType.IntType =>
      v.asInstanceOf[Int]
    case _ =>
      throw new RuntimeException(s"type error: $v is not numeric")

  def setValue(x: Any, vtype: BuiltinAstValueType): Unit =
    v = x
    builtinValueType = vtype

  def setValue(x: Any): Unit = x match
    case _: Int =>
      setValue(x, BuiltinAstValueType.IntType)
    case _: Float =>
      setValue(x, BuiltinAstValueType.Real)
    case _: String =>
      setValue(x, BuiltinAstValueType.StringType)
    case _ =>
      throw new RuntimeException(s"type error $x")

  def +(y: VarValue): VarValue

  def -(y: VarValue): VarValue


