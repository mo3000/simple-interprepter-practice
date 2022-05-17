package org.ball.mini
package ast

case class Assign(v: Var, expr: AstNode) extends AstNode:

  override def toString: String = s"$v = $expr"
