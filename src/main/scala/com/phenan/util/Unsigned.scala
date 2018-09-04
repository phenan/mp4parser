package com.phenan.util

object Unsigned {
  def apply (byteValue: Byte): UnsignedByte = new UnsignedByte(byteValue)
  def apply (intValue: Int): UnsignedInt = new UnsignedInt(intValue)
  def apply (longValue: Long): UnsignedLong = new UnsignedLong(longValue)
}

class UnsignedByte (val underlying: Byte) extends AnyVal {
  override def toString: String = toInt.toString

  def toInt: Int = java.lang.Byte.toUnsignedInt(underlying)

  def == (n: Int): Boolean = toInt == n
}

class UnsignedInt (val underlying: Int) extends AnyVal {
  override def toString: String = Integer.toUnsignedString(underlying)
  def toString(radix: Int): String = Integer.toUnsignedString(underlying, radix)
  def toLong: Long = Integer.toUnsignedLong(underlying)
  def toUnsignedLong: UnsignedLong = Unsigned(toLong)

  def + (n: UnsignedInt): UnsignedInt = Unsigned(this.underlying + n.underlying)
  def < (n: UnsignedInt): Boolean = Integer.compareUnsigned(this.underlying, n.underlying) < 0
  def == (n: Long): Boolean = toLong == n
}

class UnsignedLong (val underlying: Long) extends AnyVal {
  override def toString: String = java.lang.Long.toUnsignedString(underlying)
  def toString(radix: Int): String = java.lang.Long.toUnsignedString(underlying, radix)

  def + (n: UnsignedLong): UnsignedLong = Unsigned(this.underlying + n.underlying)
  def - (n: UnsignedLong): UnsignedLong = Unsigned(this.underlying - n.underlying)
  def < (n: UnsignedLong): Boolean = java.lang.Long.compareUnsigned(this.underlying, n.underlying) < 0
}