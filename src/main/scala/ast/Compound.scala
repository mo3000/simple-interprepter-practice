package org.ball.mini
package ast

import scala.collection.mutable

case class Compound(stmt: List[AstNode]) extends AstNode:
  override def toString: String = stmt.mkString("[", ",", "]")
end Compound


