package org.ball.mini

import ast.{AstValueType, Symbol}

import scala.collection.mutable

class SymbolTable(val parent: SymbolTable):

  private val symbols = mutable.HashMap[String, Symbol]()
  private val childTable = mutable.HashMap[String, SymbolTable]()


  def define(symbol: Symbol): Unit =
    symbols.put(symbol.name, symbol)

  def setValue(name: String, v: Any, t: AstValueType): Unit =
    lookup(name).setValue(v, t)

  def child(name: String): SymbolTable = childTable(name)


  def lookup(name: String): Symbol =
    var s = this
    while !symbols.contains(name) do
      if s.parent != null then
        s = s.parent
      else
        throw RuntimeException("variable '$name' undefined")
    val v = s.symbols(name)
    if v == null then
      throw RuntimeException(s"variable '$name' doesn't init")
    v
  end lookup

  def createChildTable(name: String): Unit =
    childTable.put(name, SymbolTable(this))

  def desdroyChildTable(name: String): Unit = childTable.remove(name)

  def define(name: String, paramType: AstValueType, defaultValue: Any): Unit =
    if symbols.contains(name) then
      throw RuntimeException(s"var name $name conflict")
    setValue(name, defaultValue, paramType)


  override def toString: String = symbols.toString
end SymbolTable
