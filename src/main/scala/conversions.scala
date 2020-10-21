package peano.conversions

trait FromInt[+T] {
    def fromInt(i: Int): T
}

trait ToBigInt[-T] {
    def toBigInt(t: T): BigInt
}