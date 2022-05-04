package org.ball.mini

import org.ball.mini.ast.AstNode
import org.scalatest.flatspec.AnyFlatSpec

class TestMain extends AnyFlatSpec {

  val parser = Parser()
  val visitor = Visitor()

  private def parse(s: String) =
    parser.input(s)
    parser.astTree()

  private def eval(s: String) =
    visitor.visit(parse(s))

  "value" should "equal" in {
    assert(eval("(14 + 2 - (5 + 3)) * 3 - 6 / 2") == 21)
    assert(eval("14 + -1 + 3 * -2") == 7)
    assert(eval("5---+-3") == 8)
    assert(eval("5---+-(3 + 4)- +2") == 10)
  }

}

