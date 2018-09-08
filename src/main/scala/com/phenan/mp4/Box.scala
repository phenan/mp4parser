package com.phenan.mp4

import com.phenan.util._
import IdentifierUtil._

import scala.collection.immutable.BitSet

sealed trait Box {
  def toHumanReadableString: String

  var children: List[Box] = Nil
}

sealed trait FullBox extends Box

case class FileTypeBox (majorBrand: UnsignedInt, minorVersion: UnsignedInt, compatibleBrands: List[UnsignedInt]) extends Box {
  override def toHumanReadableString: String = {
    s"FileTypeBox(majorBrand = ${toIdentifierString(majorBrand)}, minorVersion = $minorVersion, compatibleBrands = ${compatibleBrands.map(toIdentifierString).mkString("List(", ", ", ")")})"
  }
}

case class MovieBox () extends Box {
  override def toHumanReadableString: String = s"MovieBox"
}

case class MediaDataBox (data: Array[Byte]) extends Box {
  override def toHumanReadableString: String = {
    s"MediaDataBox(data = <byte array: ${data.length}bytes>)"
  }
}

case class MovieHeaderBox
(version: UnsignedByte, creationTime: UnsignedLong, modificationTime: UnsignedLong, timeScale: UnsignedInt, duration: UnsignedLong,
 rate: Int, volume: Short, matrix: Array[Int], nextTrackId: UnsignedInt) extends FullBox
{
  override def toHumanReadableString: String = {
    s"MovieHeaderBox(version = $version, creationTime = $creationTime, modificationTime = $modificationTime, timeScale = $timeScale, duration = $duration, " +
    s"rate = $rate, volume = $volume, matrix = ((${matrix(0)}, ${matrix(1)}, ${matrix(2)}), (${matrix(3)}, ${matrix(4)}, ${matrix(5)}), (${matrix(6)}, ${matrix(7)}, ${matrix(8)})), nextTrackId = $nextTrackId)"
  }
}

case class TrackBox () extends Box {
  override def toHumanReadableString: String = s"TrackBox()"
}

case class TrackHeaderBox
(version: UnsignedByte, flags: BitSet, creationTime: UnsignedLong, modificationTime: UnsignedLong,
 trackId: UnsignedInt, duration: UnsignedLong, layer: Short, alternateGroup: Short, volume: Short,
 matrix: Array[Int], width: UnsignedInt, height: UnsignedInt) extends FullBox
{
  override def toHumanReadableString: String = {
    s"TrackHeaderBox(version = $version, flags = ${BitSetUtil.toBitArrayString(flags)}, creationTime = $creationTime, modificationTime = $modificationTime, trackId = $trackId, duration = $duration, layer = $layer, alternateGroup = $alternateGroup, volume = $volume, " +
    s"matrix = ((${matrix(0)}, ${matrix(1)}, ${matrix(2)}), (${matrix(3)}, ${matrix(4)}, ${matrix(5)}), (${matrix(6)}, ${matrix(7)}, ${matrix(8)})), width = $width, height = $height)"
  }
}

case class TrackReferenceBox () extends Box {
  override def toHumanReadableString: String = "TrackReferenceBox()"
}

sealed trait TrackReferenceTypeBox extends Box {
  def trackIds: List[UnsignedInt]
}

case class TrackReferenceTypeBoxHint (trackIds: List[UnsignedInt]) extends TrackReferenceTypeBox {
  override def toHumanReadableString: String = s"TrackReferenceTypeBoxHint(trackIds = ${trackIds.mkString("List(", ", ", ")")})"
}

case class TrackReferenceTypeBoxCdsc (trackIds: List[UnsignedInt]) extends TrackReferenceTypeBox {
  override def toHumanReadableString: String = s"TrackReferenceTypeBoxCdsc(trackIds = ${trackIds.mkString("List(", ", ", ")")})"
}

case class MediaBox () extends Box {
  override def toHumanReadableString: String = "MediaBox()"
}

// language code is actually padding (1 bit) + character (5 bit) * 3, but this class holds it as a short value (16 bit)
case class MediaHeaderBox
(version: UnsignedByte, creationTime: UnsignedLong, modificationTime: UnsignedLong,
 timeScale: UnsignedInt, duration: UnsignedLong, language: Short) extends FullBox
{
  override def toHumanReadableString: String = {
    s"MediaHeaderBox(version = $version, creationTime = $creationTime, modificationTime = $modificationTime, timeScale = $timeScale, duration = $duration, language = $language)"
  }
}

case class HandlerBox (version: UnsignedByte, handlerType: UnsignedInt, name: String) extends FullBox {
  override def toHumanReadableString: String = {
    s"HandlerBox(version = $version, handlerType = ${toIdentifierString(handlerType)}, name = $name)"
  }
}

case class MediaInformationBox () extends Box {
  override def toHumanReadableString: String = "MediaInformationBox()"
}

case class VideoMediaHeaderBox (version: UnsignedByte, graphicsMode: UnsignedShort, opColor: Array[UnsignedShort]) extends FullBox {
  override def toHumanReadableString: String = {
    s"VideoMediaHeaderBox(version = $version, graphicsMode = $graphicsMode, opColor = { ${opColor(0)}, ${opColor(1)}, ${opColor(2)} })"
  }
}

case class SoundMediaHeaderBox (version: UnsignedByte, balance: Short) extends FullBox {
  override def toHumanReadableString: String = {
    s"SoundMediaHeaderBox(version = $version, balance = $balance)"
  }
}

case class HintMediaHeaderBox (version: UnsignedByte, maxPDUSize: UnsignedShort, avgPDUSize: UnsignedShort, maxBitRate: UnsignedInt, avgBitRate: UnsignedInt) extends FullBox {
  override def toHumanReadableString: String = {
    s"HintMediaHeaderBox(version = $version, maxPDUSize = $maxPDUSize, avgPDUSize = $avgPDUSize, maxBitRate = $maxBitRate, avgBitRate = $avgBitRate)"
  }
}

case class NullMediaHeaderBox (version: UnsignedByte, flags: BitSet) extends FullBox {
  override def toHumanReadableString: String = {
    s"NullMediaHeaderBox(version = $version, flags = ${BitSetUtil.toBitArrayString(flags)})"
  }
}

case class UnknownBox (boxType: UnsignedInt, data: Array[Byte]) extends Box {
  override def toHumanReadableString: String = {
    s"UnknownBox(boxType = ${toIdentifierString(boxType)}($boxType), data = <byte array: ${data.length}bytes>)"
  }
}
