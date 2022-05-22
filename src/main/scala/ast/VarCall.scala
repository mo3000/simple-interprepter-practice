package org.ball.mini
package ast

case class VarCall(name: String) extends AstNode:
  override def toString: String = name
