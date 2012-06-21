/** Encodes floating point value as a stream of bits.
 * max is the maximum absolute value that v may take.
 * abs(v) should not be greater than max. */
class FloatEncoder(v: Double, max: Double)  {
    assert(v.abs <= max, "Absolute value greater than max")

    def encode: Stream[Int] = encode(max, 0)

    def encode(width: Double, midpoint: Double): Stream[Int] = {
        val w = width/2
        if (v < midpoint) {
            0 #:: encode(w, midpoint - w)
        } else {
            1 #:: encode(w, midpoint + w)
        }
    }
}

/** Maximum latitude and longitude values are different. */
class LatEncoder(lat: Double) extends FloatEncoder(lat, 90)
class LonEncoder(lon: Double) extends FloatEncoder(lon, 180)

/** Interleaving of latitude and longitude bit streams.
 * Longitude is even bits, latitude is odd bits. */
class CoordEncoder(var lat: Double, var lon: Double) {
    val latBits = new LatEncoder(lat).encode
    val lonBits = new LonEncoder(lon).encode

    /* Stream((1,2),(3,4),(5,6)...) => Stream(1,2,3,4,5,6,...) */
    def explode(z: Stream[(Int,Int)]): Stream[Int] = z match {
        case Stream.Empty => Stream.Empty
        case (a, b) #:: rest => a #:: b #:: explode(rest)
    }

    def encode: Stream[Int] = explode(lonBits zip latBits)
}

/** Base 32 Encoder from a Stream of bits (Ints). */
object Base32 {
    val BASE32 = "0123456789bcdefghjkmnpqrstuvwxyz"
    val POWERS = Array(16,8,4,2,1)

    def encode(bits: Stream[Int]): Stream[Char] = {
        val x = (bits zip POWERS).collect{case(a,b) => a*b}.sum
        BASE32(x) #:: encode(bits drop POWERS.size)
    }
}

/** Geohash procedure:
 * Produce Coordinate bits by interleaving latitude and longitude bits
 * Base32 encode the bit stream. */
class Geohash(lat: Double, lon: Double) {
    assert(lat.abs <= 90, "Latitude not between -90 and 90")
    assert(lon.abs <= 180, "Longitude not between -180 and 180")

    val hash = Base32.encode(new CoordEncoder(lat, lon).encode)

    val DEFAULT_PRECISION = 12

    def toString(len: Int): String = hash.take(len).toList.mkString

    override def toString = toString(DEFAULT_PRECISION)

    val neighbors: Char => String = {
    }
}

object Test {
    def main(args: Array[String]) {
        val hash = new Geohash(42.6, -5.6)
        println(hash)
    }
}
