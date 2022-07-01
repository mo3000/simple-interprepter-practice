package org.ball.mini
package ast

case class Procedure(name: String, params: List[Param], body: Option[Block]) extends AstNode
