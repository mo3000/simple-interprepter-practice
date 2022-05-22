package org.ball.mini
package ast

case class Program(name: String, block: Block) extends AstNode:

  override def toString: String = s"BEGIN $name:\n$block\nEND $name"

