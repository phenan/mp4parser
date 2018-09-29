package com.phenan.util

import java.io.{EOFException, File}

import scala.annotation.tailrec
import scala.util._

class ByteParser [+T] (private val perform: ByteReader => Try[T]) {
  def map [U] (f: T => U): ByteParser[U] = new ByteParser[U](r => perform(r).map(f))

  def flatMap [U] (f: T => ByteParser[U]): ByteParser[U] = new ByteParser[U]({ r =>
    perform(r).flatMap(t => f(t).perform(r))
  })

  def times (n: Int): ByteParser[List[T]] = times(n.toLong)

  def timesU (n: UnsignedInt): ByteParser[List[T]] = times(n.toLong)

  def times (n: Long): ByteParser[List[T]] = new ByteParser[List[T]]({ r =>
    timesPerform(n, r, Nil)
  })

  @tailrec
  private def timesPerform [R >: T] (n: Long, reader: ByteReader, result: List[R]): Try[List[R]] = {
    if (n > 0) perform(reader) match {
      case Success(t) => timesPerform(n - 1, reader, t :: result)
      case Failure(e) => Failure(e)
    }
    else Success(result.reverse)
  }


  def filter (f: T => Boolean): ByteParser[T] = new ByteParser[T]({ r =>
    perform(r).filter(f)
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
