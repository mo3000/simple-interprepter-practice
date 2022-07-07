package org.ball.mini
package ast

case class Param(varname: String, vartype: AstValueType, defaultValue: Any = null) extends AstNode
