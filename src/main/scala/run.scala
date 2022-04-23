package org.ball.mini

import org.ball.mini.Interpreter

object run extends App {
  val inter = new Interpreter()
  inter.input("11 / 10 * 4 + 3 - 4 * 15")
  println(inter.eval())
}
