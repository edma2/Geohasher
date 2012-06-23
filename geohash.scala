object Geohash {
    def approximate(v: Double, min: Double, max: Double): Stream[Int] = {
        val midPoint = min + (max - min)/2
        if (v < midPoint) 0 #:: approximate(v, min, midPoint)
        else 1 #:: approximate(v, midPoint, max)
    }

    def interleave[T](a: Stream[T], b: Stream[T]): Stream[T] = {
        if (a.isEmpty) b
        else a.head #:: interleave(b, a.tail)
    }

    def encode(lat: Double, lon: Double): Stream[Char] = {
        Base32.encode(interleave(
            approximate(lon, -180, 180),
            approximate(lat, -90, 90)))
    }

    def encode(lat: Double, lon: Double, len: Int): String = {
        encode(lat, lon) take len mkString
    }
}

object Base32 {
    val BASE32 = "0123456789bcdefghjkmnpqrstuvwxyz"
    val POWERS = Array(16,8,4,2,1)

    def encode(bits: Stream[Int]): Stream[Char] = {
        val x = bits.zip(POWERS).collect{case(a,b) => a*b}.sum
        BASE32(x) #:: encode(bits drop POWERS.size)
    }
}

object Test {
    def main(args: Array[String]) {
        println(Geohash.encode(42.6, -5.6, 5))
    }
}
