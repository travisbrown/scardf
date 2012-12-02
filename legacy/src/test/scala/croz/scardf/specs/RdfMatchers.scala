package net.croz.scardf.specs

import net.croz.scardf.Model
import org.specs2.matcher._
import org.specs2.matcher.Matchers._

trait RdfMatchers {
  def be_=~( m1: Model ) = beIsomorphicWith( m1 )

  def beIsomorphicWith(m1: Model): Matcher[Model] = (
    (m2: Model) => (m1.local =~ m2.local),
    (m2: Model) => m1 + " is isomorphic with " + m2,
    (m2: Model) => m1 + " is not isomorphic with " + m2
  )
}

