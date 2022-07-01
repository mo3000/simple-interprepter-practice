package org.ball.mini

import org.ball.mini.ast.*

import scala.collection.mutable
import org.ball.mini.{Op, Token, BuiltinValue}

import scala.annotation.tailrec

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

  @tailrec
  private def skipWhiteSpace(): Unit =
    while (!isEof && current.isWhitespace)
      advance()
    if (!isEof && current == '{') {
      while (!isEof && current != '}')
        advance()
      advance()
      skipWhiteSpace()
    }

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
          if current == '.' then
            if isFloat then
              throw new RuntimeException("encounter second dot")
            else
              isFloat = true
          advance()
        end while
        val str = text.substring(start, pos)
        if !isFloat then
          BuiltinValue.Integer(str.toInt)
        else
          BuiltinValue.Real(str.toFloat)
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
      case ',' =>
        advance()
        Op.Comma
      case v if v.isLetter =>
        val begin = pos
        while !isEof && (current.isLetterOrDigit || current == '_') do
          advance()
        end while
        val word = text.substring(begin, pos)
        word.toUpperCase match
          case "BEGIN" =>
            Keyword.Begin
          case "END" =>
            Keyword.End
          case "VAR" =>
            Keyword.VarDecl
          case "INTEGER" =>
            Keyword.TypeInt
          case "REAL" =>
            Keyword.TypeReal
          case "PROGRAM" =>
            Keyword.Program
          case "PROCEDURE" =>
            Keyword.Procedure
          case "DIV" =>
            Op.IntDiv
          case _ =>
            VariableOrProc(word)
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
      arr.append(get_next_token())
      skipWhiteSpace()
    end while
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
    while pos < parsedToken.length && Set(Op.Mul, Op.Div, Op.IntDiv).contains(currentToken) do
      val t = currentToken
      advance()
      t match
        case Op.Mul =>
          v = BinOp(Op.Mul, v, factor())
        case Op.Div =>
          v = BinOp(Op.Div, v, factor())
        case Op.IntDiv =>
          v = BinOp(Op.IntDiv, v, factor())
      end match
    end while
    v

  def compound(): Compound =
    eat(Keyword.Begin)
    val c = Compound(stmtList())
    eat(Keyword.End)
    c


  def assign(): Assign =
    val name = variable()
    eat(Op.ASSIGN)
    Assign(name, expr())

  def variable(): String =
    val token = eat[VariableOrProc](currentToken)
    token.name


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

  def block(): Block =
    val vars = if (currentToken == Keyword.VarDecl) decl() else List.empty
    val comp = compound()
    if currentToken == Keyword.Dot then
      assert(pos == parsedToken.length - 1)
    else
      eat(Keyword.Semicolon)
    Block(vars, comp)
  end block

  def varNameList(): List[String] =
    val q = mutable.Queue[String]()
    while currentToken != Op.Colon do
      val v = eat[VariableOrProc](currentToken)
      q.enqueue(v.name)
      if currentToken == Op.Comma then
        advance()
    end while
    q.toList

  def decl(): List[VarDecl] =
    eat(Keyword.VarDecl)
    val q = mutable.Queue[VarDecl]()
    while currentToken != Keyword.Begin do
      val varnames = varNameList()
      eat(Op.Colon)
      val typeVal = if currentToken == Keyword.TypeInt then
        BuiltinAstValueType.IntType
      else if currentToken == Keyword.TypeReal then
        BuiltinAstValueType.Real
      else throw new RuntimeException(s"type def error: $currentToken")
      advance()
      eat(Keyword.Semicolon)
      q.addAll(varnames.map(v => VarDecl(v, typeVal)))
    end while
    q.toList
  end decl


  def program(): Program =
    eat(Keyword.Program)
    val name = currentToken.asInstanceOf[VariableOrProc].name
    advance()
    eat(Keyword.Semicolon)
    Program(name, block())

  def procedure(): Procedure =
    eat(Keyword.Procedure)
    val name = eat[VariableOrProc](currentToken).name
    if currentToken == Op.LParen then
      advance()
      val arr = mutable.Queue[Param]()
      while currentToken != Op.RParen do
        val vname = eat[VariableOrProc](currentToken).name
        eat(Op.Colon)
        if currentToken == Keyword.TypeInt then
          arr.enqueue(Param(vname, BuiltinAstValueType.IntType))
        else
          assert(currentToken == Keyword.TypeReal)
          arr.enqueue(Param(vname, BuiltinAstValueType.Real))
        advance()
        if currentToken == Op.Comma then
          advance()
      end while
      eat(Keyword.Semicolon)
      Procedure(
        name, arr.toList,
        if currentToken == Keyword.VarDecl then Some(block())
        else Option.empty)
    else
      eat(Keyword.Semicolon)
      Procedure(name, List.empty, Option.empty)
    end if
  end procedure




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
      case VariableOrProc(name) =>
        advance()
        VarCall(name)
      case _ =>
        Num(eat[BuiltinValue.Integer](currentToken))


  def astTree(): AstNode =
    val parsedExpr = tokenList()
    parsedToken = parsedExpr
    pos = 0
    program()
}