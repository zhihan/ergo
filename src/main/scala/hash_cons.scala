/** Scala implementation of hash cons */

package my.util

trait HashedType {
  def equal(other: HashedType): Boolean ;
  def hash(me: HashedType) : Int 
}

class HashTable[T<:HashedType] {
}

