package net.croz.scardf

import org.joda.time.LocalDate
import org.specs2.mutable._
import org.specs2.specification.Scope
import PeopleVocabulary._

class ScardfTest extends SpecificationWithJUnit {
  "NodeBag" should {
    "throw exception on taking one node from empty bag" in {
      NodeBag().oneNode must throwA[ NoSuchElementException ]
    }
    "sort" in {
      NodeBag( Lit( "a" ), Lit( "b" ), Lit( "a" ) ).sorted must_== NodeBag( Lit( "a" ), Lit( "a" ), Lit( "b" ) )
    }
    "do equals" in {
      NodeBag( Lit( "a" ) ) must_== NodeBag( Lit( "a" ) )
      NodeBag( Lit( "a" ) ) must_!= NodeBag( Lit( "a" ), Lit( "a" ) )
      NodeBag( Lit( "a" ), Lit( "b" ), Lit( "a" ) ) must_== NodeBag( Lit( "a" ), Lit( "a" ), Lit( "b" ) )
    }
  }
  "Constructed graph" should {
    "extract property value" in new modelScope {
      Given( Name( jdoe ) ) must_== Lit( "John" )
    }
    "assign value to property" in new modelScope {
      Weight( jdoe ) = 88
      Weight( jdoe ) must_== Lit( 88 )
      //(Name-Family)( Res( "anna" ) ) = "Doe"
    }
    "read path" in new modelScope {
      jdoe/Name/Given/asString must_== "John"
      jdoe/Height/asInt must_== 167
    }
    "handle multiple-node results" in new modelScope {
      jdoe/Spouse must beEmpty
      jdoe/Spouse/asRes.option must_== None
      jdoe/Spouse/Name/Family must beEmpty
      jdoe/Spouse/Name/Family/asString.default( "(unknown)" ) must_== "(unknown)"
      jdoe/Likes/asRes.set must_== Set( Swimming, Science )
    }
    "test boolean value" in new modelScope {
      ( jdoe/IsMale? ) must_== true
      jdoe has Height -> 167 must_== true
      ( jdoe( Likes -> Science )? ) must_== true
    }
    "read date" in new modelScope {
      jdoe/Birthday/asLocalDate must_== new LocalDate( 1977, 7, 27 )
    }
    "read collections" in new modelScope {
      (jdoe/Children/asRdfList).toList must_== List( Res( "anna" ), Res( "bob" ) )
    }
    "sparql query heighest" in new modelScope {
      val selectHighest = Sparql select 'person where( ('person, Height, 'h) ) orderBy desc( 'h ) limit 1
      val results = selectHighest from model
      results.solutions must_== List( Map( QVar( "person" ) -> jdoe ) )
    }
    "sparql query select X" in new modelScope {
      Sparql selectX asRes where( (X, Height, 167) ) from model must_== Some( jdoe )
    }
  }
  /*
  "read graph" should {
    val turtleSrc = """
@prefix :        <person:> .
<example:jdoe>
      a :Person ;
      :Birthday "1977-07-27";
      :Children (<example:anna> <example:bob>) ;
      :Height 167;
      :IsMale true;
      :Likes  :Swimming , :Science ;
      :Name   [ :Family "Doe";
                :Given  "John"
              ] .
<example:anna> a :Person; :Name [:Family "Doe"; :Given "Anna"].
<example:bob> a :Person; :Name [:Family "Doe"; :Given "Bob"].
"""
    import PeopleVocabulary._
    val m = new Model
    m.jModel.read( new java.io.StringReader( turtleSrc ), null, "TURTLE" )
    "" in {
      val rlist = m.getRes( "example:jdoe" )/Children/asRdfList
      rlist.toList must_== List( m.getRes( "example:anna" ), m.getRes( "example:bob" ) )
    }
    "" in {
      println( m.getRes( "example:jdoe" )/Children/asRdfList/Name/Family )
    }
  }*/
}

trait modelScope extends Scope {
  implicit val model = new Model() //withPrefix "example:"
  val jdoe = Res( "jdoe" ) a Person state(
    Name -> Anon(
      Given -> "John",
      Family -> "Doe"
    ),
    Birthday -> "1977-07-27",
    Height -> 167,
    IsMale -> true,
    Likes -> All( Swimming, Science ),
    Children -> RdfList( Res( "anna" ), Res( "bob" ) )
  )
}

