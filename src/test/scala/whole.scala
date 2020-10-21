package peano.whole

import peano.conversions._

import peano.nat.Nat
import peano.nat.{given _}

import algebra.ring.Ring
import algebra.Order
import cats.kernel.UnboundedEnumerable
import cats.implicits._

import org.scalatest._
import flatspec._
import matchers._

import scala.language.implicitConversions // this is needed because of all the implicit conversions in ScalaTest

class WholeSpec extends AnyFlatSpec with should.Matchers {

    val convert = summon[(FromInt[Whole[Nat]] & ToBigInt[Whole[Nat]])]
    val order = summon[Order[Whole[Nat]]]
    val enumerable = summon[UnboundedEnumerable[Whole[Nat]]]
    val ring = summon[Ring[Whole[Nat]]]

    "Wholes" should "be constructed from Ints" in {
        import peano.nat.Nat._
        convert.fromInt(0) shouldBe Whole(Z, Z)
        convert.fromInt(3) shouldBe Whole(S(S(S(Z))), Z)
        convert.fromInt(-3) shouldBe Whole(Z, S(S(S(Z))))
        convert.toBigInt(convert.fromInt(21)).intValue shouldBe 21
    }

    it should "convert to Ints" in {
        import peano.nat.Nat._
        convert.toBigInt(Whole(Z, Z)).intValue shouldBe 0
        convert.toBigInt(Whole(S(S(S(S(S(Z))))), Z)).intValue shouldBe 5
        convert.toBigInt(Whole(Z, S(S(S(S(S(Z))))))).intValue shouldBe -5
        convert.fromInt(convert.toBigInt(Whole(Z, S(S(Z)))).intValue) shouldBe Whole(Z, S(S(Z)))
    }

    "Order of Wholes" should "order Wholes" in {
        val x = convert.fromInt(3)
        val y = convert.fromInt(-1)

        order.lteqv(x, y) shouldBe false
        order.lteqv(x, x) shouldBe true
        order.lteqv(y, y) shouldBe true
        order.lteqv(y, x) shouldBe true

        enumerable.previous(enumerable.next(y)) eqv y shouldBe true
        enumerable.next(enumerable.previous(y)) eqv y shouldBe true
    }

    "Ring of Whole" should "add Wholes" in {
        val x = convert.fromInt(3)
        val y = convert.fromInt(-7)
        val z = convert.fromInt(0)

        ring.plus(x, y) eqv convert.fromInt(-4) shouldBe true
        ring.plus(y, x) eqv convert.fromInt(-4) shouldBe true
        ring.plus(x, x) eqv convert.fromInt(6) shouldBe true
        ring.plus(y, y) eqv convert.fromInt(-14) shouldBe true
        ring.plus(x, z) eqv x shouldBe true
        ring.plus(y, z) eqv y shouldBe true
        ring.plus(z, x) eqv x shouldBe true
        ring.plus(z, y) eqv y shouldBe true
    }

    it should "subtract Wholes" in {
        val x = convert.fromInt(3)
        val y = convert.fromInt(-7)
        val z = convert.fromInt(0)

        ring.minus(x, y) eqv convert.fromInt(10) shouldBe true
        ring.minus(y, x) eqv convert.fromInt(-10) shouldBe true
        ring.minus(x, x) eqv z shouldBe true
        ring.minus(y, y) eqv z shouldBe true
        ring.minus(x, z) eqv x shouldBe true
        ring.minus(y, z) eqv y shouldBe true
        ring.minus(z, x) eqv convert.fromInt(-3) shouldBe true
        ring.minus(z, y) eqv convert.fromInt(7) shouldBe true
    }

    it should "multiply Wholes" in {
        val x = convert.fromInt(3)
        val y = convert.fromInt(-7)
        val z = convert.fromInt(0)

        ring.times(x, y) eqv convert.fromInt(-21) shouldBe true
        ring.times(y, x) eqv convert.fromInt(-21) shouldBe true
        ring.times(x, x) eqv convert.fromInt(9) shouldBe true
        ring.times(y, y) eqv convert.fromInt(49) shouldBe true
        ring.times(x, z) eqv z shouldBe true
        ring.times(y, z) eqv z shouldBe true
        ring.times(z, x) eqv z shouldBe true
        ring.times(z, y) eqv z shouldBe true
    }

}
