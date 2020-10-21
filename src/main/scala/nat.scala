package peano.nat

import peano.conversions._

import algebra.ring.CommutativeRig
import algebra.Order
import cats.kernel.LowerBoundedEnumerable

import scala.math.BigInt

enum Nat:
    case Z
    case S(n: Nat)

class NatOrder extends Order[Nat] with LowerBoundedEnumerable[Nat]:
    @scala.annotation.tailrec
    final def compare(n1: Nat, n2: Nat): Int =
        (n1, n2) match
            case (Nat.Z, Nat.Z) => 0
            case (Nat.Z, _) => -1
            case (_, Nat.Z) => 1
            case (Nat.S(x), Nat.S(y)) => compare(x, y)

    val order = this

    val minBound: Nat = Nat.Z

    def next(n: Nat): Nat = Nat.S(n)

    def partialPrevious(n: Nat): Option[Nat] =
        n match
            case Nat.Z => None
            case Nat.S(x) => Some(x)
end NatOrder

given (Order[Nat] & LowerBoundedEnumerable[Nat]) = new NatOrder

class NatRig extends CommutativeRig[Nat]:
    val zero: Nat = Nat.Z

    @scala.annotation.tailrec
    final def plus(n1: Nat, n2: Nat): Nat =
        (n1, n2) match
            case (x, Nat.Z) => x
            case (Nat.Z, y) => y
            case (Nat.S(x), y) => plus(x, Nat.S(y))

    val one: Nat = Nat.S(Nat.Z)

    def times(n1: Nat, n2: Nat): Nat =
        @scala.annotation.tailrec
        def loop(c: Nat, r: Nat): Nat =
            (c, r) match
                case (Nat.Z, y) => y
                case (Nat.S(x), y) => loop(x, plus(y, n2))
        loop(n1, zero)
end NatRig

given CommutativeRig[Nat] = new NatRig

given (FromInt[Nat] &  ToBigInt[Nat]) = new FromInt[Nat] with ToBigInt[Nat] {
    def fromInt(i: Int): Nat =
        if i < 0 then throw peano.errors.NonNat(i)
        else if i == 0 then Nat.Z
        else 0.until(i).foldLeft(Nat.Z)((n, _) => Nat.S(n))

    def toBigInt(n: Nat): BigInt =
        @scala.annotation.tailrec
        def loop(n1: Nat, c: BigInt): BigInt =
            n1 match
                case Nat.Z => c
                case Nat.S(x) => loop(x, c + 1)
        loop(n, 0)
}


