package com.phenan.util

import java.io._

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class ByteReader private (in: DataInputStream) {
  def u1: Try[UnsignedByte] = read(Unsigned(1L), Unsigned(in.readByte()))
  def u2: Try[UnsignedShort] = read(Unsigned(2L), Unsigned(in.readShort()))
  def u4: Try[UnsignedInt] = read(Unsigned(4L), Unsigned(in.readInt()))
  def u8: Try[UnsignedLong] = read(Unsigned(8L), Unsigned(in.readLong()))

  def s2: Try[Short] = read(Unsigned(2L), in.readShort())
  def s4: Try[Int] = read(Unsigned(4L), in.readInt())

  def bytes(size: UnsignedLong): Try[Array[Byte]] = {
    // Note: Java only supports array size up to Int.MaxValue
    val bs = new Array[Byte](size.underlying.toInt)
    read(size, in.read(bs)).map(_ => bs)
  }

  def readNullEndedString: Try[String] = Try(readNullEndedString(ArrayBuffer.empty))

  private def readNullEndedString(buf: ArrayBuffer[Byte]): String = {
    val ch = in.readByte()
    pos = pos + Unsigned(1L)
    if (ch == '\0') new String(buf.toArray, "UTF-8")
    else {
      buf += ch
      readNullEndedString(buf)
    }
  }

  def readUntilEoF: Try[Array[Byte]] = {
    val out = new ByteArrayOutputStream
    val bs = new Array[Byte](1024)
    Try(readUntilEoF(out, bs))
  }

  private def readUntilEoF (out: ByteArrayOutputStream, buffer: Array[Byte]): Array[Byte] = {
    val len = in.read(buffer)
    if (len < 0) out.toByteArray
    else {
      out.write(buffer, 0, len)
      readUntilEoF(out, buffer)
    }
  }

  def currentPosition: UnsignedLong = pos

  private def read [T] (size: UnsignedLong, run: => T): Try[T] = Try {
    val t = run
    pos = pos + size
    t
  }

  private var pos: UnsignedLong = Unsigned(0L)
}

object ByteReader {
  def open (file: File): Try[ByteReader] = Try {
    new ByteReader(new DataInputStream(new FileInputStream(file)))
  }
}