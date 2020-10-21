package peano.errors

case class NonNat(i: Int) extends IllegalArgumentException(s"$i is not a natural number")