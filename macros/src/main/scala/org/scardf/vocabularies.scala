package org.scardf

import java.io.InputStream
import scala.language.experimental.macros
import scala.reflect.macros.Context

object VocabularyFactory {
  def fromSchemaResource(path: String): Vocabulary = macro fromSchemaResource_impl
   
  def fromSchemaResource_impl(c: Context)(path: c.Expr[String]) = {
    import c.universe._

    val Literal(Constant(resource: String)) = path.tree

    val stream = this.getClass.getResourceAsStream(resource)
    val doc = xml.XML.load(stream)

    val ns = (doc \ "Ontology" \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}about").text

    val anon = c.fresh()

    c.Expr(Block(
      List(
        ClassDef(
          Modifiers(Flag.FINAL),
          newTypeName(anon),
          List(),
          Template(
            List(Select(Select(Ident("org"), "scardf"), newTypeName("Vocabulary"))),
            emptyValDef,
            List(
              DefDef(
                Modifiers(),
                nme.CONSTRUCTOR,
                List(),
                List(List()),
                TypeTree(),
                Block(
                  List(
                    Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
                    List(Literal(Constant(ns))))
                  ),
                  Literal(Constant(()))
                )
              )
            )
          )
        )
      ),
      Apply(Select(New(Ident(newTypeName(anon))), nme.CONSTRUCTOR), List())
    ))
  }
}

/*object RDFS extends Vocabulary( "http://www.w3.org/2000/01/rdf-schema#" ) {
  val Resource = RDFS\"Resource"
  val Literal = RDFS\"Literal"
  val Class = RDFS\"Class"
  val Datatype = RDFS\"Datatype"
  val Container = RDFS\"Container"
  val ContainerMembershipProperty = RDFS\"ContainerMembershipProperty"
  val subClassOf = prop("subClassOf")
  val subPropertyOf = prop("subPropertyOf")
  val domain = prop("domain")
  val range = prop("range")
  val label = propStr( "label" )
  val comment = propStr( "comment" )
  val member = RDFS\"member"
  val seeAlso = RDFS\"seeAlso"
  val isDefinedBy = RDFS\"isDefinedBy"
}*/

