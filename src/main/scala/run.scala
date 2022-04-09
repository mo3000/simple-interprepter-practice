package org.ball.mini

import org.ball.mini.Interpreter

object run extends App {
  val inter = new Interpreter()
  inter.input("1 + 2 + 3 - 10 * 4")
  println(inter.eval())
}
