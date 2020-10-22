package peano.ratio

import peano.conversions._

import algebra.ring.{CommutativeRig, CommutativeRing, Field}
import algebra.{Eq, Order}

import scala.util.{Try, Success}

final case class Ratio[N: CommutativeRing : Order](num: N, den: N)

object Ratio:
    def apply[N: CommutativeRing : Order](num: N, den: N): Ratio[N] =
        val norder = summon[Order[N]]
        val nring = summon[CommutativeRing[N]]
        if nring.isZero(den) then throw peano.errors.DivByZero
        else if norder.lt(den, nring.zero) then new Ratio(nring.negate(num), nring.negate(den))
        else new Ratio(num, den)

def embed[N: CommutativeRing : Order](n: N): Ratio[N] =
    Ratio(n, summon[CommutativeRing[N]].one)
    
class RatioOrder[N: CommutativeRing : Order] extends Order[Ratio[N]]:
    val norder = summon[Order[N]]
    val nring = summon[CommutativeRing[N]]
    
    def compare(r1: Ratio[N], r2: Ratio[N]): Int =
        norder.compare(nring.times(r1.num, r2.den), nring.times(r1.den, r2.num))
end RatioOrder

given [N: CommutativeRing : Order] as Order[Ratio[N]] = new RatioOrder[N]

class RatioField[N: CommutativeRing : Order](using Eq[Ratio[N]]) extends Field[Ratio[N]]:
    val nring = summon[CommutativeRing[N]]

    def negate(r: Ratio[N]): Ratio[N] =
        Ratio(nring.negate(r.num), r.den)

    val zero: Ratio[N] = Ratio(nring.zero, nring.one)

    def plus(r1: Ratio[N], r2: Ratio[N]): Ratio[N] =
        Ratio(
            nring.plus(
                nring.times(r1.num, r2.den),
                nring.times(r1.den, r2.num)
            ),
            nring.times(r1.den, r2.den)
        )

    val one: Ratio[N] = Ratio(nring.one, nring.one)

    def times(r1: Ratio[N], r2: Ratio[N]): Ratio[N] =
        Ratio(nring.times(r1.num, r2.num), nring.times(r1.den, r2.den))

    def div(r1: Ratio[N], r2: Ratio[N]): Ratio[N] = {
        if isZero(r2) then throw peano.errors.DivByZero
        else Ratio(nring.times(r1.num, r2.den), nring.times(r1.den, r2.num))
    }
end RatioField

given [N: CommutativeRing : Order](using Eq[Ratio[N]]) as Field[Ratio[N]] = new RatioField[N]

given [N: FromInt : ToBigInt : CommutativeRing : Order] as (FromString[Ratio[N]] & ToString[Ratio[N]]) = new FromString[Ratio[N]] with ToString[Ratio[N]] {
    def fromString(s: String): Ratio[N] =
        val nconvert = summon[FromInt[N]]
        val nring = summon[CommutativeRing[N]]
        val norder = summon[Order[N]]
        s.split("/").toList.map(x => Try.apply(nconvert.fromInt(x.toInt))) match {
            case Success(n) :: Nil => embed(n)
            case Success(n) :: Success(d) :: Nil => Ratio(n, d)
            case _ => throw peano.errors.NonRatio(s)
        }

    def toString(r: Ratio[N]): String =
        val convert = summon[ToBigInt[N]]
        val nring = summon[CommutativeRing[N]]
        val norder = summon[Order[N]]
        val num = convert.toBigInt(r.num).toString
        val den = convert.toBigInt(r.den).toString
        if nring.isZero(r.num) || nring.isOne(r.den) then num
        else s"$num/$den"
}
