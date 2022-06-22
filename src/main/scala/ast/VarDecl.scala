package org.ball.mini
package ast

case class VarDecl(name: String, typeVal: BuiltinAstValueType) extends AstNode:
  override def toString: String = name
  
end VarDecl
