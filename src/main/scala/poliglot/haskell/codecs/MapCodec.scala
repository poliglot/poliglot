package poliglot.haskell.codecs

import scodec.bits.BitVector
import scodec.{Decoder, Err, Codec}
import poliglot.haskell.Types
import scalaz.\/

class MapCodec[A, B](key: Codec[A], value: Codec[B]) extends Codec[Types.Map[A, B]] {
  val size = scodec.codecs.int64
  val pair = key ~ value

  override def encode(s: Types.Map[A, B]): \/[Err, BitVector] =
    \/.left(Err("not implemented"))

  override def decode(buffer: BitVector) =
    size.decode(buffer).flatMap { case (rest, n) =>
      val items = Decoder.decodeCollect[List, (A, B)](pair, Some(n.toInt))(rest)
      items.map { case (rest2, items2) => (rest2, items2.toMap) }
    }
}
