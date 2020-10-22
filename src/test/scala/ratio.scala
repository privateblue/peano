package peano.ratio

import peano.conversions._

import peano.nat.Nat
import peano.nat.{given _}
import peano.whole.Whole
import peano.whole.{given _}

import algebra.ring.{CommutativeRing, Field}
import algebra.Order

import org.scalatest._
import wordspec._
import matchers._

import scala.language.implicitConversions // this is needed because of all the implicit conversions in ScalaTest

class RatioSpec extends AnyWordSpec with should.Matchers {
    
    val convert = summon[(FromString[Ratio[Whole[Nat]]] & ToString[Ratio[Whole[Nat]]])]
    val order = summon[Order[Ratio[Whole[Nat]]]]
    val field = summon[Field[Ratio[Whole[Nat]]]]

    "Ratios" should {
        
        "be constructed from and convert back to Strings" in {
            convert.toString(convert.fromString("-19/4")) shouldBe "-19/4"
            convert.toString(convert.fromString("19/-4")) shouldBe "-19/4"
            convert.toString(convert.fromString("-19/-4")) shouldBe "19/4"
            convert.toString(convert.fromString("19/4")) shouldBe "19/4"
            convert.toString(convert.fromString("19")) shouldBe "19"
            convert.toString(convert.fromString("-19")) shouldBe "-19"
            convert.toString(convert.fromString("0/3")) shouldBe "0"
            an [peano.errors.NonRatio] should be thrownBy convert.fromString("2b.")
            an [peano.errors.NonRatio] should be thrownBy convert.fromString("2/3/4")
            an [peano.errors.NonRatio] should be thrownBy convert.fromString("")
            an [peano.errors.DivByZero.type] should be thrownBy convert.fromString("19/0")
        }
        
    }

    "Order of Ratios" should {

        val r1 = convert.fromString("2/3")
        val r2 = convert.fromString("-4/5")
        val r3 = convert.fromString("0")
        
        "order Ratios" in {
            order.lteqv(r2, r1) shouldBe true
            order.lteqv(r1, r2) shouldBe false
            order.lteqv(r2, r3) shouldBe true
            order.lteqv(r1, r3) shouldBe false
            order.lteqv(r1, r1) shouldBe true
            order.lteqv(r2, r2) shouldBe true
            order.lteqv(r3, r3) shouldBe true
        }
        
    }
    
    "Field of Ratios" should {
        
        val r1 = convert.fromString("2/3")
        val r2 = convert.fromString("4/5")
        val r3 = convert.fromString("-2/3")
        val r4 = convert.fromString("0")

        "add Ratios" in {
            convert.toString(field.plus(r1, r2)) shouldBe "22/15"
            convert.toString(field.plus(r3, r2)) shouldBe "2/15"
            convert.toString(field.plus(r3, r3)) shouldBe "-12/9"
            convert.toString(field.plus(r4, r1)) shouldBe "2/3"
            convert.toString(field.plus(r4, r3)) shouldBe "-2/3"
            convert.toString(field.plus(r1, r4)) shouldBe "2/3"
            convert.toString(field.plus(r3, r4)) shouldBe "-2/3"
        }

        "subtract Ratios" in {
            convert.toString(field.minus(r1, r2)) shouldBe "-2/15"
            convert.toString(field.minus(r3, r2)) shouldBe "-22/15"
            convert.toString(field.minus(r3, r3)) shouldBe "0"
            convert.toString(field.minus(r4, r1)) shouldBe "-2/3"
            convert.toString(field.minus(r4, r3)) shouldBe "2/3"
            convert.toString(field.minus(r1, r4)) shouldBe "2/3"
            convert.toString(field.minus(r3, r4)) shouldBe "-2/3"
        }

        "multiply Ratios" in {
            convert.toString(field.times(r1, r2)) shouldBe "8/15"
            convert.toString(field.times(r3, r2)) shouldBe "-8/15"
            convert.toString(field.times(r3, r3)) shouldBe "4/9"
            convert.toString(field.times(r4, r1)) shouldBe "0"
            convert.toString(field.times(r4, r3)) shouldBe "0"
            convert.toString(field.times(r1, r4)) shouldBe "0"
            convert.toString(field.times(r3, r4)) shouldBe "0"
        }

        "divide Ratios" in {
            convert.toString(field.div(r1, r2)) shouldBe "10/12"
            convert.toString(field.div(r3, r2)) shouldBe "-10/12"
            convert.toString(field.div(r3, r3)) shouldBe "6/6"
            convert.toString(field.div(r4, r1)) shouldBe "0"
            convert.toString(field.div(r4, r3)) shouldBe "0"
            an [peano.errors.DivByZero.type] should be thrownBy field.div(r1, r4)
            an [peano.errors.DivByZero.type] should be thrownBy field.div(r3, r4)
        }
        
    }

}
