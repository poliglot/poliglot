package poliglot.haskell.codecs

import scodec.{Decoder, Err, Codec}
import scalaz.\/
import scodec.bits.BitVector
import poliglot.haskell.Types

class ListCodec[T](item: Codec[T]) extends Codec[Types.List[T]] {
  val size = scodec.codecs.int64

  override def encode(s: Types.List[T]): \/[Err, BitVector] =
    \/.left(Err("not implemented"))

  override def decode(buffer: BitVector) =
    size.decode(buffer).flatMap { case (rest, n) =>
      Decoder.decodeCollect[List, T](item, Some(n.toInt))(rest)
    }
}
