package peano.nat

import peano.conversions._

import algebra.ring.Rig
import algebra.Order
import cats.kernel.LowerBoundedEnumerable
import cats.implicits._

import org.scalatest._
import flatspec._
import matchers._
import scala.language.implicitConversions // this is needed because of all the implicit conversions in ScalaTest

class NatSpec extends AnyFlatSpec with should.Matchers {

    import Nat._

    val convert = summon[(FromInt[Nat] & ToBigInt[Nat])]
    val order = summon[Order[Nat]]
    val enumerable = summon[LowerBoundedEnumerable[Nat]]
    val rig = summon[Rig[Nat]]

    "Nats" should "be constructed from Ints" in {
        convert.fromInt(0) shouldBe Z
        convert.fromInt(3) shouldBe S(S(S(Z)))
        convert.toBigInt(convert.fromInt(21)).intValue shouldBe 21
    }

    it should "convert to Ints" in {
        convert.toBigInt(Z).intValue shouldBe 0
        convert.toBigInt(S(S(S(S(S(Z)))))).intValue shouldBe 5
        convert.fromInt(convert.toBigInt(S(S(Z))).intValue) shouldBe S(S(Z))
    }

    "Order of Nats" should "order Nats" in {
        val x = convert.fromInt(3)
        val y = convert.fromInt(2)

        order.lteqv(x, y) shouldBe false
        order.lteqv(x, x) shouldBe true
        order.lteqv(y, x) shouldBe true

        enumerable.partialPrevious(enumerable.next(x)) eqv Some(x) shouldBe true
        enumerable.partialPrevious(x).map(enumerable.next) eqv Some(x) shouldBe true
    }

    "Rig of Nats" should "add Nats" in {
        val x = convert.fromInt(3)
        val y = convert.fromInt(7)
        val z = convert.fromInt(10)

        rig.plus(x, y) eqv z shouldBe true
        rig.plus(y, x) eqv z shouldBe true
        rig.plus(x, Z) eqv x shouldBe true
        rig.plus(Z, x) eqv x shouldBe true
    }

    it should "multiply Nats" in {
        val x = convert.fromInt(3)
        val y = convert.fromInt(2)
        val z = convert.fromInt(6)

        rig.times(x, y) eqv z shouldBe true
        rig.times(y, x) eqv z shouldBe true
        rig.times(x, Z) eqv Z shouldBe true
        rig.times(Z, x) eqv Z shouldBe true
    }

}
