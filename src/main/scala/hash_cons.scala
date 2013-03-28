/** Scala implementation of hash cons */

package my.util

import scala.collection.mutable.ArrayBuffer // array

trait HashedType {
  def equal(other: HashedType): Boolean ;
  def hash : Int 
}

object Gentag {
  // Initialize 
  var r = 1
  def apply() = {
    r +=1
    r
  }
  def reset() = {
    // Don't reset gentag to -1
    // As 0 and 1 are reserved for concrete values
    r = 1
  }
}

class HashCons[T<:HashedType] (size:Int) {
  var table = createTable(size)
  def createTable(size:Int) = {
    val sz = if (size<7) 7 else size
    val t = ArrayBuffer[ArrayBuffer[(T,Int)]]()
    
    for (i <- 1 to sz) {
      t.append(ArrayBuffer[(T,Int)]())
    }
    t
  }

  var limit = 3  // bucket length is limited by limit * table.length
  
  // Iterate over all entries in the 2-D table
  private def iter(tab:ArrayBuffer[ArrayBuffer[(T,Int)]], f:((T,Int))=>Unit) {
    tab.foreach{ row =>
      row.foreach { v => f(v) }
    }
  }

  private def nextSize(n:Int) = (3*n/2 + 3)
  def resize() {
    // Resize table prevents a hash bucket grows too big
    
    val newSize = nextSize(table.length)
    // Initialize
    val newTable = createTable(newSize)
    val oldTable = table
    
    table = newTable
    iter(oldTable, (x:(T,Int)) => {
      addNode(x)
      } )
        
    limit += 2 // increase the limit
  }
  
  private def addNodeAtRow(d:(T,Int), index:Int) {
    // Insert the bdd node at the corresponding index in the
    // table. 
    table(index).append(d)
    
    if (table(index).length > table.length * limit) {
      // bucket is significantly longer than table
      resize()
    }
  }

  def addNode(d:(T,Int)) {
    // Compute hash code and use it modulo table length
    // as the index of the entry
    addNodeAtRow(d, d._1.hash % table.length)
  }
  


  def count() = {
    var i = 0
    table.foreach { row => i += row.length}
    i
  }
  def hashCons(v:T): (T,Int) = {
    val k = v.hash 
    val index = k % table.length
    val bucket = table(index)
    val d = bucket.find( _ match {  
      case (x,_) => v.equal(x)
    } )
    d match {
      case None => {
        val newNode = (v, Gentag());
        bucket.append(newNode)
        newNode
      }
      case Some(e) => e
    }
  }

  def stat() = {
    "Table size:" + table.length + "; " +
    "count:" + count() + "; " +
    "limit:" + limit
  }
 

}

