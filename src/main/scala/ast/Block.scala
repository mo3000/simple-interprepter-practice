package org.ball.mini
package ast

case class Block(decls: List[VarDecl], comp: Compound) extends AstNode:

  override def toString: String = s"${decls.mkString(",")}\n$comp"
