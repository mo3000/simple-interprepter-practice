package org.ball.mini


sealed trait Token {
  def isOp: Boolean = false
  def isValue: Boolean = false
  def isObj: Boolean = false

  def value: AnyVal | String | Op | Null
}

enum Op(val v: String) extends Token {
  case Plus extends Op("+")
  case Minus extends Op("-")
  case Mul extends Op("*")
  case Div extends Op("/")

  override def isOp: Boolean = true
  override def value: String = v
}

enum Value[T <: AnyVal](val v: T) extends Token {
  case Integer(i: Int) extends Value[Int](i)
  case FloatNum(f: Float) extends Value[Float](f)

  override def isValue: Boolean = true
  override def value: T = v
}

case class Eof() extends Token {
  override def value: Char = 0
}
