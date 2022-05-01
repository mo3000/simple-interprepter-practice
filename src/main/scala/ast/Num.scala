package org.ball.mini
package ast


case class Num(val num: Value[?]) extends AstNode:

  def value: num.valueType = num.value

  override def toString: String = s"${num.value}"
