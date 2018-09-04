package com.phenan.util

import java.io.{EOFException, File}

import scala.util._

class ByteParser [+T] (private val perform: ByteReader => Try[T]) {
  def map [U] (f: T => U): ByteParser[U] = new ByteParser[U](r => perform(r).map(f))

  def flatMap [U] (f: T => ByteParser[U]): ByteParser[U] = new ByteParser[U]({ r =>
    perform(r).flatMap(t => f(t).perform(r))
  })

  def times (n: Int): ByteParser[List[T]] = new ByteParser[List[T]]({ r =>
    if (n > 0) for {
      head <- perform(r)
      tail <- times(n - 1).perform(r)
    } yield head :: tail
    else Success(Nil)
  })

  def until (position: UnsignedLong): ByteParser[List[T]] = new ByteParser[List[T]]({ r =>
    if (r.currentPosition < position) for {
      head <- perform(r)
      tail <- until(position).perform(r)
    } yield head :: tail
    else Success(Nil)
  })

  def untilEoF: ByteParser[List[T]] = new ByteParser[List[T]]({ r =>
    val res = for {
      head <- perform(r)
      tail <- untilEoF.perform(r)
    } yield head :: tail

    res.recover {
      case _: EOFException => Nil
    }
  })

  def parse (file: File): Try[T] = ByteReader.open(file).flatMap(perform)
}
