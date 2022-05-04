package org.ball.mini
package ast

case class UnaryOp(val op: Op, val node: AstNode) extends AstNode