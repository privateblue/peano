package peano.whole

import peano.nat
import nat.{given _}
import nat.Nat

import algebra.ring.{Rig, Ring}
import algebra.Order
import cats.kernel.{LowerBoundedEnumerable, UnboundedEnumerable}

final case class Whole(a: Nat, b: Nat)

def fromNat(n: Nat)(implicit natRig: Rig[Nat]): Whole =
    Whole(n, natRig.zero)

def fromInt(i: Int)(implicit natRig: Rig[Nat], wholeRing: Ring[Whole]): Whole =
    i match
        case x if x >= 0 => fromNat(nat.fromInt(x))
        case x if x < 0 => wholeRing.negate(fromNat(nat.fromInt(-x)))

def toBigInt(z: Whole): BigInt =
    nat.toBigInt(z.a) - nat.toBigInt(z.b)

class WholeOrder(using
    natOrder: Order[Nat],
    natEnumerable: LowerBoundedEnumerable[Nat],
    natRig: Rig[Nat]
) extends Order[Whole] with UnboundedEnumerable[Whole]:
    def compare(z1: Whole, z2: Whole): Int =
        natOrder.compare(natRig.plus(z1.a, z2.b), natRig.plus(z1.b, z2.a))

    val order = this

    def next(z: Whole): Whole =
        Whole(natEnumerable.next(z.a), z.b)

    def previous(z: Whole): Whole =
        Whole(z.a, natEnumerable.next(z.b))
end WholeOrder

given (Order[Whole] & UnboundedEnumerable[Whole]) = new WholeOrder

class WholeRing(implicit natRig: Rig[Nat]) extends Ring[Whole]:
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
end WholeRing

given Ring[Whole] = new WholeRing
