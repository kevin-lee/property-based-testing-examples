package io.kevinlee.property_based_testing

import hedgehog.runner._
import hedgehog._

/**
  * @author Kevin Lee
  * @since 2018-10-13
  */
object MyAppSpecWithHedgehog extends Properties {
  override def tests: List[Prop] = List(
    example("add(1, 9) should return 10", testOnePlusNine)
  , property("add(a, b) should return a + b", testAdd).withTests(200)
  , property("add(Int, Int): has identity element", testIdentity)
  , property("add(Int, Int): test commutative property", testCommutative)
  , property("add(Int, Int): test associative property", testAssociative)
  )

  def testOnePlusNine: Result =
    MyApp.add(1, 9) ==== 10

  def testAdd: Property = for {
    x <- Gen.int(Range.linear(1, 1000)).forAll
    y <- Gen.int(Range.linear(1, 1000)).forAll
//    _ = println(s"x: $x, y: $y")
  } yield MyApp.add(x, y) ==== x + y

  def testIdentity: Property = for {
    x <- Gen.int(Range.linear(-1000, 1000)).forAll
//    _ = println(s"x: $x")
    /* x + 0 == x */
  } yield MyApp.add(x, 0) ==== x

  def testCommutative: Property = for {
    x <- Gen.int(Range.linear(-1000, 1000)).forAll
    y <- Gen.int(Range.linear(-1000, 1000)).forAll
//    _ = println(s"x: $x / y: $y")
    /* x + y == y + x */
  } yield MyApp.add(x, y) ==== MyApp.add(y, x)

  def testAssociative: Property = for {
    x <- Gen.int(Range.linear(-1000, 1000)).forAll
    y <- Gen.int(Range.linear(-1000, 1000)).forAll
    z <- Gen.int(Range.linear(-1000, 1000)).forAll
//    _ = println(s"x: $x / y: $y / z: $z")
    /* ((x + y) + z) ==== (x + (y + z)) */
  } yield  MyApp.add(MyApp.add(x, y), z)  ==== MyApp.add(x, MyApp.add(y, z))

}
