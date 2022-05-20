package org.ball.mini


sealed trait Token {
  def isOp: Boolean = false
  def isValue: Boolean = false
  def isObj: Boolean = false
  def isVar: Boolean = false
  def isKeyword: Boolean = false

  def value: Int | Float | String | Op | Null
}


enum Op(val v: String) extends Token {
  case Plus extends Op("+")
  case Minus extends Op("-")
  case Mul extends Op("*")
  case Div extends Op("/")
  case LParen extends Op("(")
  case RParen extends Op(")")
  case EQ extends Op("==")
  case LT extends Op("<")
  case LTE extends Op("<=")
  case GTE extends Op(">=")
  case GT extends Op(">")
  case Colon extends Op(":")
  case ASSIGN extends Op(":=")


  override def isOp: Boolean = true
  override def value: String = v
}

enum Keyword(val name: String) extends Token {
  case Begin extends Keyword("BEGIN")
  case End extends Keyword("END")
  case Dot extends Keyword(".")
  case Semicolon extends Keyword(";")

  override def value: String = name

  override def isKeyword = true
}



case class Variable(name: String) extends Token:

  override def isVar: Boolean = true

  override def value: String = name

end Variable



enum Value(val v: Int | Float | String) extends Token {
  case Integer(i: Int) extends Value(i)
  case FloatNum(f: Float) extends Value(f)
  case StringVal(s: String) extends Value(s)
  
  override def isValue: Boolean = true
  override def value: Int | Float | String = v
}


case class Eof() extends Token {
  override def value: Int = 0
}
