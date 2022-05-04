package org.ball.mini

import org.ball.mini.Parser

object run extends App {
  val inter = new Parser()
  inter.input("(14 + 2 - (5 + 3)) * 3 - 6 / 2")
  val tree = inter.astTree()
  println(Visitor().visit(tree))
}
