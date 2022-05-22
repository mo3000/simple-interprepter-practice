package org.ball.mini
package ast

case class VarDecl(name: String, typeVal: ValueType) extends AstNode:
  override def toString: String = name
  
end VarDecl
