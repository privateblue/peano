package peano

import algebra.ring.Rig
import algebra.Order
import cats.kernel.LowerBoundedEnumerable
import cats.implicits._

import org.scalatest._
import flatspec._
import matchers._

class NatSpec extends AnyFlatSpec with should.Matchers {

    "Nats" should "be constructed from Ints" in {
        Nat.fromInt(0) shouldBe Z
        Nat.fromInt(3) shouldBe S(S(S(Z)))
        Nat.toBigInt(Nat.fromInt(21)).intValue shouldBe 21
    }

    it should "convert to Ints" in {
        Nat.toBigInt(Z).intValue shouldBe 0
        Nat.toBigInt(S(S(S(S(S(Z)))))).intValue shouldBe 5
        Nat.fromInt(Nat.toBigInt(S(S(Z))).intValue) shouldBe S(S(Z))
    }

    "Order of Nats" should "order Nats" in {
        val x = Nat.fromInt(3)
        val y = Nat.fromInt(2)

        Order[Nat].lteqv(x, y) shouldBe false
        Order[Nat].lteqv(x, x) shouldBe true
        Order[Nat].lteqv(y, x) shouldBe true

        val enum = implicitly[LowerBoundedEnumerable[Nat]]

        enum.partialPrevious(enum.next(x)) eqv Some(x) shouldBe true
        enum.partialPrevious(x).map(enum.next) eqv Some(x) shouldBe true
    }

    "Rig of Nats" should "add Nats" in {
        val x = Nat.fromInt(3)
        val y = Nat.fromInt(7)
        val z = Nat.fromInt(10)

        Rig[Nat].plus(x, y) eqv z shouldBe true
        Rig[Nat].plus(y, x) eqv z shouldBe true
        Rig[Nat].plus(x, Z) eqv x shouldBe true
        Rig[Nat].plus(Z, x) eqv x shouldBe true
    }

    it should "multiply Nats" in {
        val x = Nat.fromInt(3)
        val y = Nat.fromInt(2)
        val z = Nat.fromInt(6)

        Rig[Nat].times(x, y) eqv z shouldBe true
        Rig[Nat].times(y, x) eqv z shouldBe true
        Rig[Nat].times(x, Z) eqv Z shouldBe true
        Rig[Nat].times(Z, x) eqv Z shouldBe true
    }

}