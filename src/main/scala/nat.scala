package peano

import algebra.ring.Rig
import algebra.Order
import cats.kernel.LowerBoundedEnumerable

import scala.math.BigInt

sealed trait Nat
final case object Z extends Nat
final case class S(n: Nat) extends Nat

object Nat extends NatInstances {
    def z: Nat = Z
    def s(n: Nat): Nat = S(n)

    def fromInt(i: Int): Nat =
        if (i < 0) throw new IllegalArgumentException("Not a natural number")
        else if (i == 0) Z
        else 0.until(i).foldLeft(Nat.z)((n, _) => Nat.s(n))

    def toBigInt(n: Nat): BigInt = {
        @scala.annotation.tailrec
        def loop(n1: Nat, c: BigInt): BigInt =
            n1 match {
                case Z => c
                case S(x) => loop(x, c + 1)
            }
        loop(n, 0)
    }

}

trait NatInstances {
    implicit val natOrder: Order[Nat] with LowerBoundedEnumerable[Nat] =
        new NatOrder

    implicit val natRig: Rig[Nat] =
        new NatRig
}

class NatOrder extends Order[Nat] with LowerBoundedEnumerable[Nat] { self =>
    @scala.annotation.tailrec
    final def compare(n1: Nat, n2: Nat): Int =
        (n1, n2) match {
            case (Z, Z) => 0
            case (Z, _) => -1
            case (_, Z) => 1
            case (S(x), S(y)) => compare(x, y)
        }

    val order = self

    val minBound: Nat = Z

    def next(n: Nat): Nat = S(n)

    def partialPrevious(n: Nat): Option[Nat] =
        n match {
            case Z => None
            case S(x) => Some(x)
        }
}

class NatRig extends Rig[Nat] {
    val zero: Nat = Z

    @scala.annotation.tailrec
    final def plus(n1: Nat, n2: Nat): Nat =
        (n1, n2) match {
            case (x, Z) => x
            case (Z, y) => y
            case (S(x), y) => plus(x, Nat.s(y))
        }

    val one: Nat = S(Z)

    def times(n1: Nat, n2: Nat): Nat = {
        @scala.annotation.tailrec
        def loop(c: Nat, r: Nat): Nat =
            (c, r) match {
                case (Z, y) => y
                case (S(x), y) => loop(x, plus(y, n2))
            }
        loop(n1, zero)
    }
}