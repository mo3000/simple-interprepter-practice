package org.ball.mini
package ast

case class Assign(name: String, expr: AstNode) extends AstNode:

  override def toString: String = s"$name = $expr"
