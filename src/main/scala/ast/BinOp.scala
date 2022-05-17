package org.ball.mini
package ast

import java.util.function.BinaryOperator

case class BinOp(op: Op, left: AstNode, right: AstNode)
  extends AstNode:
  
  override def toString: String =
    s"$left ${op.value} $right"
  
end BinOp
