package org.ball.mini

import org.ball.mini.Interpreter

object run extends App {
  val inter = new Interpreter()
  inter.input("(14 + 2 - (5 + 3)) * 3 - 6 / 2")
  println(inter.eval())
}
