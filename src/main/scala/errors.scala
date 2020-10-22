package peano.errors

case class NonNat(i: Int) extends IllegalArgumentException(s"$i is not a natural number")
case class NonRatio(s: String) extends IllegalArgumentException(s"$s is not a rational number")
case object DivByZero extends IllegalArgumentException("Cannot divide by zero")