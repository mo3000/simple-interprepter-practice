package org.ball.mini
package ast


case class Num(num: Value) extends AstNode:

  def value: Int | Float | String = num.value

  override def toString: String = s"${num.value}"
