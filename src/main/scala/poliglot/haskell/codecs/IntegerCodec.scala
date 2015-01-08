package poliglot.haskell.codecs

import scodec.{Err, Codec}
import scodec.bits.BitVector

import scalaz.{\/, \/-, -\/}

import poliglot.haskell.Types

class IntegerCodec extends Codec[Types.Integer] {
  override def encode(s: Types.Integer): \/[Err, BitVector] =
    \/.left(Err("not implemented"))

  override def decode(buffer: BitVector) = {
    buffer.acquire(8) match {
      case Left(e) => \/.left(Err(e))
      case Right(b) =>
        val newBuf = buffer.drop(8)
        val tag = b.toInt()

        if (tag == 0) /* Small integer */
          newBuf.acquire(32) match {
            case Left(e) => \/.left(Err(e))
            case Right(b2) => \/.right((newBuf.drop(32), b.toInt()))
          }
        else {
          newBuf.acquire(8) match {
            case Left(e) => \/.left(Err(e))
            case Right(b) =>
              val signValue = b.toInt()
              val newNewBuf = newBuf.drop(8)

              val digits = new ListCodec(scodec.codecs.int8).decode(newNewBuf)

              digits match {
                case \/-((rest, digitsList)) =>
                  /* TODO This is not implemented correctly. */
                  val v = digitsList.reverse.foldLeft(0) { (acc, cur) => (acc << 8) + cur }
                  \/.right(rest, if (signValue == 1) v else -v)
                case -\/(err) => \/.left(Err("Unknown error"))
              }
          }
        }
    }
  }
}