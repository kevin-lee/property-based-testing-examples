package io.kevinlee.property_based_testing

import hedgehog.runner._
import hedgehog._

/**
  * @author Kevin Lee
  * @since 2018-10-13
  */
object MyAppSpecWithHedgehog extends Properties {
  override def tests: List[Prop] = List(
//    Prop("add(a, b) should return a + b", testAdd.withTests(200))
    Prop("add(Int, Int): has identity element", testIdentity)
  , Prop("add(Int, Int): test commutative property", testCommutative)
  , Prop("add(Int, Int): test associative property", testAssociative)
  )

  def testAdd: Property[Unit] = for {
    x <- Gen.int(Range.linear(1, 1000)).forAll
    y <- Gen.int(Range.linear(1, 1000)).forAll
//    _ = println(s"x: $x, y: $y")
    _ <- MyApp.add(x, y) ==== x + y
  } yield ()

  def testIdentity: Property[Unit] = for {
    x <- Gen.int(Range.linear(-1000, 1000)).forAll
//    _ = println(s"x: $x")
    /* x + 0 == x */
    _ <- MyApp.add(x, 0) ==== x
  } yield ()

  def testCommutative: Property[Unit] = for {
    x <- Gen.int(Range.linear(-1000, 1000)).forAll
    y <- Gen.int(Range.linear(-1000, 1000)).forAll
//    _ = println(s"x: $x / y: $y")
    /* x + y == y + x */
    _ <- MyApp.add(x, y) ==== MyApp.add(y, x)
  } yield ()

  def testAssociative: Property[Unit] = for {
    x <- Gen.int(Range.linear(-1000, 1000)).forAll
    y <- Gen.int(Range.linear(-1000, 1000)).forAll
    z <- Gen.int(Range.linear(-1000, 1000)).forAll
//    _ = println(s"x: $x / y: $y / z: $z")
    /* ((x + y) + z) ==== (x + (y + z)) */
    _ <- MyApp.add(MyApp.add(x, y), z)  ==== MyApp.add(x, MyApp.add(y, z))
  } yield ()
}
