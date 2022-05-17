package org.ball.mini

import org.ball.mini.Parser

object run extends App {
  val inter = new Parser()
  inter.input("""
      BEGIN
        BEGIN
          number := 2;
          a := number;
          b := 10 * a + 10 * number / 4;
          c := a - - b
        END;
        x := 11;
      END.
      """.stripMargin)
  val tree = inter.astTree()
//  println(Visitor().visit(tree))
  println(tree)
}
