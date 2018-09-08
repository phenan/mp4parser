package com.phenan.mp4

import java.io.File

import com.phenan.util._

import scala.util._

import scala.collection.immutable.BitSet

object Mp4Parsers extends ByteParsers {
  def all: ByteParser[List[Box]] = box.untilEoF

  def box: ByteParser[Box] = for {
    pos      <- currentPosition
    size     <- u4
    boxType  <- u4
    boxSize  <- boxSize(size)
    result   <- boxBody(pos, boxSize, boxType)
    children <- childBoxes(pos, boxSize)
  } yield {
    result.children = children
    result
  }

  private def fullBox [B <: FullBox] (initialPosition: UnsignedLong, size: UnsignedLong, bodyParser: (UnsignedLong, UnsignedLong, UnsignedByte, BitSet) => ByteParser[B]): ByteParser[B] = for {
    version <- u1
    flags   <- bytes(Unsigned(3))
    result  <- bodyParser(initialPosition, size, version, BitSetUtil.fromByteArray(flags))
  } yield result

  private def boxSize (size: UnsignedInt): ByteParser[UnsignedLong] = {
    if (size == 1) u8
    else pure(size.toUnsignedLong)
  }

  private def boxBody (initialPosition: UnsignedLong, size: UnsignedLong, boxType: UnsignedInt): ByteParser[Box] = boxType.underlying match {
    case 0x66747970 =>  // 'ftyp'
      fileTypeBox(initialPosition, size)
    case 0x6d6f6f76 =>  // 'moov'
      pure(MovieBox())
    case 0x6d646174 =>  // 'mdat'
      mediaDataBox(initialPosition, size)
    case 0x6d766864 =>  // 'mvhd'
      fullBox(initialPosition, size, movieHeaderBox)
    case 0x7472616b =>  // 'trak'
      pure(TrackBox())
    case 0x746b6864 =>  // 'tkhd'
      fullBox(initialPosition, size, trackHeaderBox)
    case 0x74726566 =>  // 'tref'
      pure(TrackReferenceBox())
    case 0x68696e74 =>  // 'hint'
      trackReferenceTypeBoxHint(initialPosition, size)
    case 0x63647363 =>  // 'cdsc'
      trackReferenceTypeBoxCdsc(initialPosition, size)
    case 0x6d646961 =>  // 'mdia'
      pure(MediaBox())
    case 0x6d646864 =>  // 'mdhd'
      fullBox(initialPosition, size, mediaHeaderBox)
    case 0x68646c72 =>  // 'hdlr'
      fullBox(initialPosition, size, handlerBox)
    case 0x6d696e66 =>  // 'minf'
      pure(MediaInformationBox())
    case 0x766d6864 =>  // 'vmhd'
      fullBox(initialPosition, size, videoMediaHeaderBox)
    case 0x736d6864 =>  // 'smhd'
      fullBox(initialPosition, size, soundMediaHeaderBox)
    case 0x686d6864 =>  // 'hmhd'
      fullBox(initialPosition, size, hintMediaHeaderBox)
    case 0x6e6d6864 =>  // 'nmhd'
      fullBox(initialPosition, size, nullMediaHeaderBox)
    case _ =>
      unknownBox(initialPosition, size, boxType)
  }

  private def childBoxes (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[List[Box]] = {
    box.until(initialPosition + size)
  }

  private def fileTypeBox (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[FileTypeBox] = for {
    majorBrand       <- u4
    minorVersion     <- u4
    compatibleBrands <- u4.until(initialPosition + size)
  } yield FileTypeBox(majorBrand, minorVersion, compatibleBrands)

  private def mediaDataBox (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[MediaDataBox] = for {
    data <- bytesUntil(initialPosition + size)
  } yield MediaDataBox(data)

  private def movieHeaderBox (initialPosition: UnsignedLong, size: UnsignedLong, version: UnsignedByte, flags: BitSet): ByteParser[MovieHeaderBox] = {
    if (version == 1) movieHeaderBoxVer1(initialPosition, size)
    else movieHeaderBoxVer0(initialPosition, size)
  }

  private def movieHeaderBoxVer0 (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[MovieHeaderBox] = for {
    creationTime     <- u4
    modificationTime <- u4
    timeScale        <- u4
    duration         <- u4
    rate             <- s4
    volume           <- s2
    _                <- s2           // reserved
    _                <- u4.times(2)  // reserved
    matrix           <- s4.times(9)
    _                <- s4.times(6)  // pre_defined
    nextTrackId      <- u4
  } yield MovieHeaderBox(Unsigned(0), creationTime.toUnsignedLong, modificationTime.toUnsignedLong, timeScale, duration.toUnsignedLong, rate, volume, matrix.toArray, nextTrackId)

  private def movieHeaderBoxVer1 (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[MovieHeaderBox] = for {
    creationTime     <- u8
    modificationTime <- u8
    timeScale        <- u4
    duration         <- u8
    rate             <- s4
    volume           <- s2
    _                <- s2           // reserved
    _                <- u4.times(2)  // reserved
    matrix           <- s4.times(9)
    _                <- s4.times(6)  // pre_defined
    nextTrackId      <- u4
  } yield MovieHeaderBox(Unsigned(1), creationTime, modificationTime, timeScale, duration, rate, volume, matrix.toArray, nextTrackId)

  private def trackHeaderBox (initialPosition: UnsignedLong, size: UnsignedLong, version: UnsignedByte, flags: BitSet): ByteParser[TrackHeaderBox] = {
    if (version == 1) trackHeaderBoxVer1(initialPosition, size, flags)
    else trackHeaderBoxVer0(initialPosition, size, flags)
  }

  private def trackHeaderBoxVer0 (initialPosition: UnsignedLong, size: UnsignedLong, flags: BitSet): ByteParser[TrackHeaderBox] = for {
    creationTime     <- u4
    modificationTime <- u4
    trackId          <- u4
    _                <- u4           // reserved
    duration         <- u4
    _                <- u4.times(2)  // reserved
    layer            <- s2
    alternateGroup   <- s2
    volume           <- s2
    _                <- s2           // reserved
    matrix           <- s4.times(9)
    width            <- u4
    height           <- u4
  } yield TrackHeaderBox(Unsigned(0), flags, creationTime.toUnsignedLong, modificationTime.toUnsignedLong, trackId, duration.toUnsignedLong, layer, alternateGroup, volume, matrix.toArray, width, height)

  private def trackHeaderBoxVer1 (initialPosition: UnsignedLong, size: UnsignedLong, flags: BitSet): ByteParser[TrackHeaderBox] = for {
    creationTime     <- u8
    modificationTime <- u8
    trackId          <- u4
    _                <- u4           // reserved
    duration         <- u8
    _                <- u4.times(2)  // reserved
    layer            <- s2
    alternateGroup   <- s2
    volume           <- s2
    _                <- s2           // reserved
    matrix           <- s4.times(9)
    width            <- u4
    height           <- u4
  } yield TrackHeaderBox(Unsigned(1), flags, creationTime, modificationTime, trackId, duration, layer, alternateGroup, volume, matrix.toArray, width, height)

  private def trackReferenceTypeBoxHint (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[TrackReferenceTypeBoxHint] = for {
    trackIds <- u4.until(initialPosition + size)
  } yield TrackReferenceTypeBoxHint(trackIds)

  private def trackReferenceTypeBoxCdsc (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[TrackReferenceTypeBoxCdsc] = for {
    trackIds <- u4.until(initialPosition + size)
  } yield TrackReferenceTypeBoxCdsc(trackIds)

  private def mediaHeaderBox (initialPosition: UnsignedLong, size: UnsignedLong, version: UnsignedByte, flags: BitSet): ByteParser[MediaHeaderBox] = {
    if (version == 1) mediaHeaderBoxVer1(initialPosition, size)
    else mediaHeaderBoxVer0(initialPosition, size)
  }

  private def mediaHeaderBoxVer0 (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[MediaHeaderBox] = for {
    creationTime     <- u4
    modificationTime <- u4
    timeScale        <- u4
    duration         <- u4
    language         <- s2
    _                <- s2           // pre_defined
  } yield MediaHeaderBox(Unsigned(0), creationTime.toUnsignedLong, modificationTime.toUnsignedLong, timeScale, duration.toUnsignedLong, language)

  private def mediaHeaderBoxVer1 (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[MediaHeaderBox] = for {
    creationTime     <- u8
    modificationTime <- u8
    timeScale        <- u4
    duration         <- u8
    language         <- s2
    _                <- s2           // pre_defined
  } yield MediaHeaderBox(Unsigned(1), creationTime, modificationTime, timeScale, duration, language)

  private def handlerBox (initialPosition: UnsignedLong, size: UnsignedLong, version: UnsignedByte, flags: BitSet): ByteParser[HandlerBox] = for {
    _           <- u4                // pre_defined
    handlerType <- u4
    _           <- u4.times(3)       // reserved
    name        <- nullEndedString
  } yield HandlerBox(version, handlerType, name)

  private def videoMediaHeaderBox (initialPosition: UnsignedLong, size: UnsignedLong, version: UnsignedByte, flags: BitSet): ByteParser[VideoMediaHeaderBox] = for {
    graphicsMode <- u2
    opColor      <- u2.times(3)
  } yield VideoMediaHeaderBox(version, graphicsMode, opColor.toArray)

  private def soundMediaHeaderBox (initialPosition: UnsignedLong, size: UnsignedLong, version: UnsignedByte, flags: BitSet): ByteParser[SoundMediaHeaderBox] = for {
    balance <- s2
    _       <- u2   // reserved
  } yield SoundMediaHeaderBox(version, balance)

  private def hintMediaHeaderBox (initialPosition: UnsignedLong, size: UnsignedLong, version: UnsignedByte, flags: BitSet): ByteParser[HintMediaHeaderBox] = for {
    maxPDUSize <- u2
    avgPDUSize <- u2
    maxBitRate <- u4
    avgBitRate <- u4
    _          <- u4  // reserved
  } yield HintMediaHeaderBox(version, maxPDUSize, avgPDUSize, maxBitRate, avgBitRate)

  private def nullMediaHeaderBox (initialPosition: UnsignedLong, size: UnsignedLong, version: UnsignedByte, flags: BitSet): ByteParser[NullMediaHeaderBox] = {
    pure(NullMediaHeaderBox(version, flags))
  }

  private def unknownBox (initialPosition: UnsignedLong, size: UnsignedLong, boxType: UnsignedInt): ByteParser[UnknownBox] = for {
    data <- bytesUntil(initialPosition + size)
  } yield UnknownBox(boxType, data)



  private def toHumanReadableString (boxes: List[Box]): String = boxes.map(toHumanReadableString).mkString("\n")

  private def toHumanReadableString (box: Box): String = {
    if (box.children.isEmpty) box.toHumanReadableString
    else box.toHumanReadableString + "\n  " + toHumanReadableString(box.children).replaceAll("\n", "\n  ")
  }

  def main (args: Array[String]): Unit = {
    all.parse(new File("test.mp4")) match {
      case Success(v) => println(toHumanReadableString(v))
      case Failure(e) => e.printStackTrace()
    }
  }
}
