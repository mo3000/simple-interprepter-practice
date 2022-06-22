package org.ball.mini
package ast


trait Numeric
trait AstValueType

enum BuiltinAstValueType extends AstValueType:
  case IntType extends BuiltinAstValueType, Numeric
  case Real extends BuiltinAstValueType, Numeric
  case StringType extends BuiltinAstValueType
end BuiltinAstValueType


enum ExtendAstValueType extends AstValueType:
  case Func extends ExtendAstValueType
  case Class extends ExtendAstValueType
  case ClassMethod extends ExtendAstValueType
end ExtendAstValueType

