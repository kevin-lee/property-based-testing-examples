package io.kevinlee.property_based_testing

import org.scalacheck.Prop._
import org.scalacheck._

/**
  * @author Kevin Lee
  * @since 2018-09-29
  */
class MyAppSpecWithScalaCheck extends Properties("MyAppSpecWithScalaCheck") {

  property("add(Int, Int): has identity element") =
    forAll { x: Int =>
      /* x + 0 == x */
      MyApp.add(x, 0) == x
    }

  property("add(Int, Int): test commutative property") =
    forAll { (x: Int, y: Int) =>
      /* x + y == y + x */
      MyApp.add(x, y) == MyApp.add(y, x)
    }

  property("add(Int, Int): test associative property") =
    forAll { (x: Int, y: Int, z: Int) =>
      /* (x + y) + z == x + (y + z) */
      MyApp.add(MyApp.add(x, y), z) == MyApp.add(x, MyApp.add(y, z))
    }

  case class IntBetweenNegative1000And1000(value: Int)

  def genBetween1And100: Gen[IntBetweenNegative1000And1000] = for {
    i <- Gen.choose(-1000, 1000)
  } yield IntBetweenNegative1000And1000(i)

  property("add(x, y) where x and y are Int between 1 and 100: test commutative property") =
    forAll(genBetween1And100, genBetween1And100) { (x: IntBetweenNegative1000And1000, y: IntBetweenNegative1000And1000) =>
      MyApp.add(x.value, y.value) == MyApp.add(y.value, x.value)

  }
}
