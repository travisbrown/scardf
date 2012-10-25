package net.croz.scardf

import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.RDFList
import scala.collection.JavaConverters._

class RdfList( val jRdfList: RDFList, override val model: Model ) extends Res( jRdfList, model )
with scala.Seq[Node] with util.Logging
{
  def toNodeBag: NodeBag = new NodeBag( this.iterator.toList )

  def jlist: List[RDFNode] = jRdfList.asJavaList.asScala.toList

  def length = jRdfList.size
  override def iterator: Iterator[Node] = jlist.map{ n: RDFNode => Node( n ) }.iterator
  def apply( i: Int ) = Node( jRdfList.get(i) )
}

object RdfList {
  def from( l: RDFList ): RdfList = Model( l.getModel ) getRdfList l

  def from( c: Iterable[Any] )( implicit model: Model ): RdfList = 
    apply( c.toArray: _* )( model )
  
  def apply( nodes: Any* )( implicit model: Model ) = {
    val jNodes = nodes map { Node from _ jNode }
    val jList = model createList jNodes.toArray
    model getRdfList jList
  }
}
