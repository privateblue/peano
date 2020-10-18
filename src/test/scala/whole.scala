package peano

import algebra.ring.Ring
import algebra.Order
import cats.kernel.UnboundedEnumerable
import cats.implicits._

import org.scalatest._
import flatspec._
import matchers._

class WholeSpec extends AnyFlatSpec with should.Matchers {

    "Wholes" should "be constructed from Ints" in {
        Whole.fromInt(0) shouldBe Whole(Z, Z)
        Whole.fromInt(3) shouldBe Whole(S(S(S(Z))), Z)
        Whole.fromInt(-3) shouldBe Whole(Z, S(S(S(Z))))
        Whole.toBigInt(Whole.fromInt(21)).intValue shouldBe 21
    }

    it should "convert to Ints" in {
        Whole.toBigInt(Whole(Z, Z)).intValue shouldBe 0
        Whole.toBigInt(Whole(S(S(S(S(S(Z))))), Z)).intValue shouldBe 5
        Whole.toBigInt(Whole(Z, S(S(S(S(S(Z))))))).intValue shouldBe -5
        Whole.fromInt(Whole.toBigInt(Whole(Z, S(S(Z)))).intValue) shouldBe Whole(Z, S(S(Z)))
    }

    "Order of Wholes" should "order Wholes" in {
        val x = Whole.fromInt(3)
        val y = Whole.fromInt(-1)

        Order[Whole].lteqv(x, y) shouldBe false
        Order[Whole].lteqv(x, x) shouldBe true
        Order[Whole].lteqv(y, y) shouldBe true
        Order[Whole].lteqv(y, x) shouldBe true

        val enum = implicitly[UnboundedEnumerable[Whole]]

        enum.previous(enum.next(y)) eqv y shouldBe true
        enum.next(enum.previous(y)) eqv y shouldBe true
    }

    "Ring of Whole" should "add Wholes" in {
        val x = Whole.fromInt(3)
        val y = Whole.fromInt(-7)
        val z = Whole.fromInt(0)

        Ring[Whole].plus(x, y) eqv Whole.fromInt(-4) shouldBe true
        Ring[Whole].plus(y, x) eqv Whole.fromInt(-4) shouldBe true
        Ring[Whole].plus(x, x) eqv Whole.fromInt(6) shouldBe true
        Ring[Whole].plus(y, y) eqv Whole.fromInt(-14) shouldBe true
        Ring[Whole].plus(x, z) eqv x shouldBe true
        Ring[Whole].plus(y, z) eqv y shouldBe true
        Ring[Whole].plus(z, x) eqv x shouldBe true
        Ring[Whole].plus(z, y) eqv y shouldBe true
    }

    it should "subtract Wholes" in {
        val x = Whole.fromInt(3)
        val y = Whole.fromInt(-7)
        val z = Whole.fromInt(0)

        Ring[Whole].minus(x, y) eqv Whole.fromInt(10) shouldBe true
        Ring[Whole].minus(y, x) eqv Whole.fromInt(-10) shouldBe true
        Ring[Whole].minus(x, x) eqv z shouldBe true
        Ring[Whole].minus(y, y) eqv z shouldBe true
        Ring[Whole].minus(x, z) eqv x shouldBe true
        Ring[Whole].minus(y, z) eqv y shouldBe true
        Ring[Whole].minus(z, x) eqv Whole.fromInt(-3) shouldBe true
        Ring[Whole].minus(z, y) eqv Whole.fromInt(7) shouldBe true
    }

    it should "multiply Wholes" in {
        val x = Whole.fromInt(3)
        val y = Whole.fromInt(-7)
        val z = Whole.fromInt(0)

        Ring[Whole].times(x, y) eqv Whole.fromInt(-21) shouldBe true
        Ring[Whole].times(y, x) eqv Whole.fromInt(-21) shouldBe true
        Ring[Whole].times(x, x) eqv Whole.fromInt(9) shouldBe true
        Ring[Whole].times(y, y) eqv Whole.fromInt(49) shouldBe true
        Ring[Whole].times(x, z) eqv z shouldBe true
        Ring[Whole].times(y, z) eqv z shouldBe true
        Ring[Whole].times(z, x) eqv z shouldBe true
        Ring[Whole].times(z, y) eqv z shouldBe true
    }
}