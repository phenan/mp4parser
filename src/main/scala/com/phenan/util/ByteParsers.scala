package com.phenan.util

import scala.util.Success

trait ByteParsers {
  def pure [T] (n: => T): ByteParser[T] = new ByteParser[T](_ => Success(n))

  val u1: ByteParser[UnsignedByte] = new ByteParser[UnsignedByte](_.u1)
  val u2: ByteParser[UnsignedShort] = new ByteParser[UnsignedShort](_.u2)
  val u4: ByteParser[UnsignedInt] = new ByteParser[UnsignedInt](_.u4)
  val u8: ByteParser[UnsignedLong] = new ByteParser[UnsignedLong](_.u8)

  val s2: ByteParser[Short] = new ByteParser[Short](_.s2)
  val s4: ByteParser[Int] = new ByteParser[Int](_.s4)

  val nullEndedString: ByteParser[String] = new ByteParser[String](_.readNullEndedString)

  val currentPosition: ByteParser[UnsignedLong] = new ByteParser[UnsignedLong](r => Success(r.currentPosition))

  def bytes (n: UnsignedLong): ByteParser[Array[Byte]] = new ByteParser[Array[Byte]](_.bytes(n))

  def bytesUntil (pos: UnsignedLong): ByteParser[Array[Byte]] = new ByteParser[Array[Byte]]({ r =>
    if (r.currentPosition < pos) r.bytes(pos - r.currentPosition)
    else Success(Array.empty)
  })
  val bytesUntilEoF: ByteParser[Array[Byte]] = new ByteParser[Array[Byte]](_.readUntilEoF)
}
