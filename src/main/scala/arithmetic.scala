package arithmetic {
  import S99Int._

  class S99Int(val start: Int) {

    def isPrime: Boolean = (start > 1) && (primes takeWhile {(x:Int) => x*x <= start} forall {start % _ != 0})

    // 33
    def isCoprime(x:Int): Boolean = gcd(start, x) == 1

    // 34
    def totient: Int = (1 to start) filter (start isCoprime _) length
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    // 31
    val primes = Stream.cons(2, Stream.from(3,2) filter {_ isPrime})

    // 32
    def gcd(left: Int, right: Int): Int = if (right == 0) left else gcd(right, left%right)
  }
}
