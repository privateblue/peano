package peano.whole

import peano.conversions._

import algebra.ring.{Rig, Ring}
import algebra.Order
import cats.kernel.{LowerBoundedEnumerable, UnboundedEnumerable}

final case class Whole[N : Rig](a: N, b: N)

def embed[N : Rig](n: N): Whole[N] =
    Whole(n, summon[Rig[N]].zero)

class WholeOrder[N: Order : LowerBoundedEnumerable : Rig] extends Order[Whole[N]] with UnboundedEnumerable[Whole[N]]:
    val norder = summon[Order[N]]
    val nenumerable = summon[LowerBoundedEnumerable[N]]
    val nrig = summon[Rig[N]]

    def compare(z1: Whole[N], z2: Whole[N]): Int =
        norder.compare(nrig.plus(z1.a, z2.b), nrig.plus(z1.b, z2.a))

    val order = this

    def next(z: Whole[N]): Whole[N] =
        Whole(nenumerable.next(z.a), z.b)

    def previous(z: Whole[N]): Whole[N] =
        Whole(z.a, nenumerable.next(z.b))
end WholeOrder

given [N: Order : LowerBoundedEnumerable : Rig ] as (Order[Whole[N]] & UnboundedEnumerable[Whole[N]]) = new WholeOrder[N]

class WholeRing[N : Rig] extends Ring[Whole[N]]:
    val nrig = summon[Rig[N]]

    def negate(z: Whole[N]): Whole[N] =
        Whole(z.b, z.a)

    val zero: Whole[N] = Whole(nrig.zero, nrig.zero)

    def plus(z1: Whole[N], z2: Whole[N]): Whole[N] =
        Whole(nrig.plus(z1.a, z2.a), nrig.plus(z1.b, z2.b))

    val one: Whole[N] = Whole(nrig.one, nrig.zero)

    def times(z1: Whole[N], z2: Whole[N]): Whole[N] =
        Whole(
            nrig.plus(
                nrig.times(z1.a, z2.a),
                nrig.times(z1.b, z2.b)
            ),
            nrig.plus(
                nrig.times(z1.a, z2.b),
                nrig.times(z1.b, z2.a)
            )
        )
end WholeRing

given [N : Rig ] as Ring[Whole[N]] = new WholeRing[N]

given [N: FromInt : ToBigInt : Rig] as (FromInt[Whole[N]] & ToBigInt[Whole[N]]) = new FromInt[Whole[N]] with ToBigInt[Whole[N]] {
    def fromInt(i: Int): Whole[N] =
        i match
            case x if x >= 0 => embed(summon[FromInt[N]].fromInt(x))
            case x if x < 0 => summon[Ring[Whole[N]]].negate(embed(summon[FromInt[N]].fromInt(-x)))

    def toBigInt(z: Whole[N]): BigInt =
        summon[ToBigInt[N]].toBigInt(z.a) - summon[ToBigInt[N]].toBigInt(z.b)
}
