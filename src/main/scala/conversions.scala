package peano.conversions

trait FromInt[+T] {
    def fromInt(i: Int): T
}

trait ToBigInt[-T] {
    def toBigInt(v: T): BigInt
}

trait FromString[+T] {
    def fromString(s: String): T
}

trait ToString[-T] {
    def toString(v: T): String
}