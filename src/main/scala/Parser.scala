package org.ball.mini

import org.ball.mini.ast.*

import scala.collection.mutable
import org.ball.mini.{Op, Token, Value}

class Parser {

  private var text = ""
  private var pos = 0
  private var parsedToken: Array[Token] = Array.empty

  def input(text: String): Unit = {
    this.text = text
    pos = 0
  }

  private def isEof: Boolean = pos >= text.length

  private def current = text(pos)

  private def advance(): Unit = pos += 1

  private def skipWhiteSpace(): Unit =
    while !isEof && current.isWhitespace do
      advance()

  private def currentToken: Token = parsedToken(pos)

  private def peek_next: Char =
    val p = pos
    if p + 1 < text.length then
      skipWhiteSpace()
      val t = current
      pos = p
      t
    else
      0


  private def get_next_token(): Token =
    val start = pos
    current match {
      case v if v.isDigit || ((v == '+' || v == '-') && peek_next.isDigit) =>
        var isFloat = false
        while !isEof && (current.isDigit || current == '.') do
          advance()
          if v == '.' then
            if isFloat then
              throw new RuntimeException("encounter second dot")
            else
              isFloat = true
        val str = text.substring(start, pos)
        if !isFloat then
          Value.Integer(str.toInt)
        else
          Value.FloatNum(str.toFloat)
      case v if Set('+', '-', '*', '/').contains(v) =>
        advance()
        v match {
          case '+' => Op.Plus
          case '-' => Op.Minus
          case '*' => Op.Mul
          case '/' => Op.Div
        }
      case '(' =>
        advance()
        Op.LParen
      case ')' =>
        advance()
        Op.RParen
      case '=' =>
        val op = text.substring(pos, pos + 2)
        if op == "==" then
          pos += 2
          Op.EQ
        else
          throw new RuntimeException(s"unknown op $op")
      case '<' =>
        if text(pos + 1) == '=' then
          pos += 2
          Op.LTE
        else
          advance()
          Op.LT
      case v if v.isLetter =>
        val begin = pos
        while !isEof && (current.isLetterOrDigit || current == '_') do
          advance()
        end while
        text.substring(begin, pos) match
          case "BEGIN" =>
            Keyword.Begin
          case "END" =>
            Keyword.End
          case v =>
            Variable(v)
      case '.' =>
        advance()
        Keyword.Dot
      case ':' =>
        if pos + 1 < text.length && text(pos + 1) == '=' then
          pos += 2
          Op.ASSIGN
        else
          advance()
          Op.Colon
      case ';' =>
        advance()
        Keyword.Semicolon
      case v =>
        throw new RuntimeException(s"not implemented: ($v)")
    }


  def tokenList(): Array[Token] =
    val arr = mutable.ArrayBuffer[Token]()
    skipWhiteSpace()
    while !isEof do
      arr.addOne(get_next_token())
      skipWhiteSpace()
    arr.toArray

  def expr(): AstNode =
    var v = term()
    while pos < parsedToken.length && Set(Op.Plus, Op.Minus).contains(currentToken) do
      val t = currentToken
      advance()
      t match
        case Op.Plus =>
          v = BinOp(Op.Plus, v, term())
        case Op.Minus =>
          v = BinOp(Op.Minus, v, term())
    end while
    v


  def cast[T <: Token](e: Token): T =
    val trans = e.asInstanceOf[T]
    trans

  def eat[T <: Token](e: Token): T =
    val t = cast[T](e)
    advance()
    t

  def term(): AstNode =
    var v = factor()
    while pos < parsedToken.length && Set(Op.Mul, Op.Div).contains(currentToken) do
      val t = currentToken
      advance()
      t match
        case Op.Mul =>
          v = BinOp(Op.Mul, v, factor())
        case Op.Div =>
          v = BinOp(Op.Div, v, factor())
    end while
    v

  def compound(): Compound =
    eat(Keyword.Begin)
    val c = Compound(stmtList())
    eat(Keyword.End)
    c

  def assign(): Assign =
    val v = variable()
    assert(currentToken == Op.ASSIGN)
    advance()
    val right = expr()
    Assign(v, right)


  def variable(v: Variable): Var =
    advance()
    Var(v.name)

  def variable(): Var =
    val token = eat[Variable](currentToken)
    Var(token.name)


  def stmt(): AstNode =
    if currentToken == Keyword.Begin then
      compound()
    else if currentToken.isVar then
      assign()
    else
      NoOp()

  def stmtList(): List[AstNode] =
    val q = mutable.Queue[AstNode]()
    q.enqueue(stmt())
    while currentToken == Keyword.Semicolon do
      eat(Keyword.Semicolon)
      q.enqueue(stmt())
    end while
    q.toList


  def eat(word: Keyword): Unit =
    assert(currentToken == word)
    advance()

  def program(): Compound =
    val c = compound()
    assert(currentToken == Keyword.Dot)
    c

  def factor(): AstNode =
    currentToken match
      case Op.LParen =>
        advance()
        val v = expr()
        assert(currentToken == Op.RParen)
        advance()
        v
      case v if v == Op.Plus || v == Op.Minus =>
        advance()
        val node = factor()
        UnaryOp(v.asInstanceOf[Op], node)
      case Variable(name) =>
        advance()
        Var(name)
      case _ =>
        Num(eat[Value.Integer](currentToken))


  def astTree(): AstNode =
    val parsedExpr = tokenList()
    parsedToken = parsedExpr
    pos = 0
    program()
}