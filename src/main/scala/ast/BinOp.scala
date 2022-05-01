package org.ball.mini
package ast

import java.util.function.BinaryOperator

case class BinOp(val op: Op, val left: AstNode, val right: AstNode)
  extends AstNode:
  
  override def toString: String =
    s"BinOp($op, $left, $right)"
  
end BinOp
