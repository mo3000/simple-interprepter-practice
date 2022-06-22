package org.ball.mini

import ast.{AstValueType, Symbol}

import scala.collection.mutable

class SymbolTable:

  private val symbols = mutable.HashMap[String, Symbol]()


  def define(symbol: Symbol): Unit =
    symbols.put(symbol.name, symbol)

  def setValue(name: String, v: Any, t: AstValueType): Unit =
    lookup(name).setValue(v, t)


  def lookup(name: String): Symbol =
    val v = symbols(name)
    if v == null then
      throw RuntimeException(s"variable $name doesn't init")
    v
  end lookup

  override def toString: String = symbols.toString
end SymbolTable
