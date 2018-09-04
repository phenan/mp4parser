package com.phenan.util

import scala.collection.immutable.BitSet

object BitSetUtil {
  def fromByteArray (bytes: Array[Byte]): BitSet = {
    BitSet.fromBitMaskNoCopy(java.util.BitSet.valueOf(bytes).toLongArray)
  }

  def toBitArrayString (bitSet: BitSet): String = {
    val seq = (0 to bitSet.max).map { x => if (bitSet.contains(x)) "1" else "0" }
    seq.mkString("0b", "", "")
  }
}
