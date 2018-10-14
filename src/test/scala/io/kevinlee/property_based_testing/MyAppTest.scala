package io.kevinlee.property_based_testing

import org.scalatest._

/**
  * @author Kevin Lee
  * @since 2018-09-29
  */
class MyAppTest extends WordSpec with Matchers {
  /* normal unit tests */

  "add(2, 2)" should {
    "add both Int values and return 4" in {

      val expected = 4
      val actual = MyApp.add(2, 2)
      actual should be (expected)

    }
  }

}
