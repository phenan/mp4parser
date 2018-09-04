package com.phenan.mp4

import java.nio.ByteBuffer

import com.phenan.util.UnsignedInt

object IdentifierUtil {
  def toIdentifierString (id: UnsignedInt): String = {
    new String(ByteBuffer.allocate(4).putInt(id.underlying).array())
  }
}
