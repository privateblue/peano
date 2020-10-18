package peano

import algebra.ring.{Rig, Ring}
import algebra.Order
import cats.kernel.{LowerBoundedEnumerable, UnboundedEnumerable}
import cats.syntax._

final case class Whole(a: Nat, b: Nat)

object Whole extends WholeInstances {
    def fromNat(n: Nat)(implicit natRig: Rig[Nat]): Whole =
        Whole(n, natRig.zero)

    def fromInt(i: Int)(implicit natRig: Rig[Nat], wholeRing: Ring[Whole]): Whole =
        i match {
            case x if x >= 0 => fromNat(Nat.fromInt(x))
            case x if x < 0 => wholeRing.negate(fromNat(Nat.fromInt(-x)))
        }

    def toBigInt(z: Whole): BigInt =
        Nat.toBigInt(z.a) - Nat.toBigInt(z.b)
}

trait WholeInstances {
    implicit val wholeOrder: Order[Whole] with UnboundedEnumerable[Whole] =
        new WholeOrder

    implicit val wholeRing: Ring[Whole] =
        new WholeRing
}

class WholeOrder(implicit
    natOrder: Order[Nat] with LowerBoundedEnumerable[Nat],
    natRig: Rig[Nat]
) extends Order[Whole] with UnboundedEnumerable[Whole] { self =>

    def compare(z1: Whole, z2: Whole): Int =
        natOrder.compare(natRig.plus(z1.a, z2.b), natRig.plus(z1.b, z2.a))

    val order = self

    def next(z: Whole): Whole =
        Whole(natOrder.next(z.a), z.b)

    def previous(z: Whole): Whole =
        Whole(z.a, natOrder.next(z.b))
}

class WholeRing(implicit natRig: Rig[Nat]) extends Ring[Whole] {
    def negate(z: Whole): Whole =
        Whole(z.b, z.a)

    val zero: Whole = Whole(natRig.zero, natRig.zero)

    def plus(z1: Whole, z2: Whole): Whole =
        Whole(natRig.plus(z1.a, z2.a), natRig.plus(z1.b, z2.b))

    val one: Whole = Whole(natRig.one, natRig.zero)

    def times(z1: Whole, z2: Whole): Whole =
        Whole(
            natRig.plus(
                natRig.times(z1.a, z2.a),
                natRig.times(z1.b, z2.b)
            ),
            natRig.plus(
                natRig.times(z1.a, z2.b),
                natRig.times(z1.b, z2.a)
            )
        )
}