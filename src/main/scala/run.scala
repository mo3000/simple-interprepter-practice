package org.ball.mini

import org.ball.mini.Parser

object run extends App {
  val inter = new Parser()
  inter.input("""
PROGRAM Part10;
VAR
   number     : INTEGER;
   a, b, c, x : INTEGER;
   y          : REAL;

BEGIN {Part10}
   BEGIN
      number := 2;
      a := number;
      b := 10 * a + 10 * number DIV 4;
      c := a - - b
   END;
   x := 11;
   y := 20 / 7 + 3.14;
   { writeln('a = ', a); }
   { writeln('b = ', b); }
   { writeln('c = ', c); }
   { writeln('number = ', number); }
   { writeln('x = ', x); }
   { writeln('y = ', y); }
END.  {Part10}
      """.stripMargin)
  val tree = inter.astTree()
  val visitor = Visitor()
  visitor.visit(tree)
  println(visitor.dumpGlobal)
}
