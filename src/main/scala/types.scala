package my.ergo

import my.util._
/* Binary operators */
abstract class Binop 
object Plus extends Binop
object Minus extends Binop
object Multiply extends Binop
object Divide extends Binop

/** Symbol can be a name, a var, a Const int or a binary operator */
abstract class Symbol
case class Name (name:String) extends Symbol
case class Var (name:String) extends Symbol
case class BinopSymbol (op:Binop) extends Symbol
case class Const (id:Int) extends Symbol

object Symbol {
  def symbolToString: Symbol=>String = {
    case Name(n) => n
    case Var(x) => x
    case BinopSymbol(Plus) => "+"
    case BinopSymbol(Minus) => "-"
    case BinopSymbol(Multiply) => "*"
    case BinopSymbol(Divide) => "/" 
    case Const(id) => id.toString
  }
}


/** Types of symbol */
sealed abstract class SymbolType 
object SInt extends SymbolType
object SBool extends SymbolType
object SReal extends SymbolType
case class SUnknown(name:String) extends SymbolType /* A symbol whose type is not determined */

/* Convenient object */
object SymbolType {
  def typeToString: SymbolType=>String = {
    case SInt => "int"
    case SBool => "bool"
    case SReal => "real"
    case SUnknown(s) => "`"+ s
  }
}

case class SymbolView(val symb:Symbol, val stype:SymbolType) extends HashedType {
  /** Equality of symbol view is defined as 
     - For variables, they are equal if the types are identical
     - Otherwise, they are equal if their type match and the symbols are identical
  */
  def equal(other: HashedType) = {
    def eqType(t1:SymbolType, t2:SymbolType) = {
      (t1,t2) match {
        case (_,SUnknown(_)) => true
        case (SUnknown(_), _) => true
        case (SBool, SBool) => true
        case (SReal, SReal) => true
        case (SInt, SInt) => true
        case _ => false
      }
    }
    other match {
      case SymbolView(s, t) => {
        (symb, s) match {
          case (Var(_), Var(_))=> (stype == t)  /* Variables of same type */
          case (_, _) => eqType(stype, t) && (symb == s) /* Same symbol and equal type */
        }
      }
      case _ => false
    }
  }

  def hash = symb.hashCode 
  def eqType(t:SymbolType) = (stype == t)

  override def toString = Symbol.symbolToString(symb) + ":" + SymbolType.typeToString(stype)
}


class HashedSymbol(val sv:SymbolView, val tag:Int) 

object HashConSymbol {
  val tbl = new HashCons[SymbolView](1000)
  def make(s:Symbol, t: SymbolType) = {
    val (sv, tag) = tbl.hashCons( SymbolView(s,t)) 
    new HashedSymbol(sv, tag)
  }
  def equal(s1:HashedSymbol, s2:HashedSymbol) = (s1.tag == s2.tag)
  def hash(sv:HashedSymbol) = sv.tag
}


