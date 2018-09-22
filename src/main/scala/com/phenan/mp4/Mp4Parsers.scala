package com.phenan.mp4

import java.io.File
import com.phenan.util._

import scala.util._

object Mp4Parsers extends ByteParsers {
  def all: ByteParser[List[Box]] = box.untilEoF


  def box: ByteParser[Box] = makeBoxParser(boxBody)

  private def makeBoxParser [B <: Box] (bodyParser: (UnsignedLong, UnsignedLong, UnsignedInt) => ByteParser[B]): ByteParser[B] = for {
    pos      <- currentPosition
    size     <- u4
    boxType  <- u4
    boxSize  <- if (size == 1) u8 else pure(size.toUnsignedLong)
    result   <- bodyParser(pos, boxSize, boxType)
    children <- childBoxes(pos, boxSize)
  } yield {
    result.children = children
    result
  }

  private def fullBoxHeader: ByteParser[(UnsignedByte, UnsignedInt)] = for {
    version <- u1
    flags   <- u3
  } yield (version, flags)

  private def boxBody (initialPosition: UnsignedLong, size: UnsignedLong, boxType: UnsignedInt): ByteParser[Box] = boxType.underlying match {
    case 0x66747970 =>  // 'ftyp'
      fileTypeBoxBody(initialPosition, size)
    case 0x6d6f6f76 =>  // 'moov'
      pure(MovieBox())
    case 0x6d646174 =>  // 'mdat'
      mediaDataBoxBody(initialPosition, size)
    case 0x6d766864 =>  // 'mvhd'
      movieHeaderBoxBody
    case 0x7472616b =>  // 'trak'
      pure(TrackBox())
    case 0x746b6864 =>  // 'tkhd'
      trackHeaderBoxBody
    case 0x74726566 =>  // 'tref'
      pure(TrackReferenceBox())
    case 0x68696e74 =>  // 'hint'
      trackReferenceTypeBoxHint(initialPosition, size)
    case 0x63647363 =>  // 'cdsc'
      trackReferenceTypeBoxCdsc(initialPosition, size)
    case 0x6d646961 =>  // 'mdia'
      pure(MediaBox())
    case 0x6d646864 =>  // 'mdhd'
      mediaHeaderBoxBody
    case 0x68646c72 =>  // 'hdlr'
      handlerBoxBody
    case 0x6d696e66 =>  // 'minf'
      pure(MediaInformationBox())
    case 0x766d6864 =>  // 'vmhd'
      videoMediaHeaderBoxBody
    case 0x736d6864 =>  // 'smhd'
      soundMediaHeaderBoxBody
    case 0x686d6864 =>  // 'hmhd'
      hintMediaHeaderBoxBody
    case 0x6e6d6864 =>  // 'nmhd'
      nullMediaHeaderBoxBody
    case 0x64696e66 =>  // 'dinf'
      pure(DataInformationBox())
    case 0x64726566 =>  // 'dref'
      dataReferenceBoxBody
    case _ =>
      unknownBox(initialPosition, size, boxType)
  }

  private def childBoxes (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[List[Box]] = {
    box.until(initialPosition + size)
  }

  private def fileTypeBoxBody (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[FileTypeBox] = for {
    majorBrand       <- u4
    minorVersion     <- u4
    compatibleBrands <- u4.until(initialPosition + size)
  } yield FileTypeBox(majorBrand, minorVersion, compatibleBrands)

  private def mediaDataBoxBody (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[MediaDataBox] = for {
    data <- bytesUntil(initialPosition + size)
  } yield MediaDataBox(data)

  private def movieHeaderBoxBody: ByteParser[MovieHeaderBox] = for {
    (version, _)     <- fullBoxHeader
    creationTime     <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    modificationTime <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    timeScale        <- u4
    duration         <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    rate             <- s4
    volume           <- s2
    _                <- s2           // reserved
    _                <- u4.times(2)  // reserved
    matrix           <- s4.times(9)
    _                <- s4.times(6)  // pre_defined
    nextTrackId      <- u4
  } yield MovieHeaderBox(version, creationTime, modificationTime, timeScale, duration, rate, volume, matrix.toArray, nextTrackId)

  private def trackHeaderBoxBody: ByteParser[TrackHeaderBox] = for {
    (version, flags) <- fullBoxHeader
    creationTime     <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    modificationTime <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    trackId          <- u4
    _                <- u4           // reserved
    duration         <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    _                <- u4.times(2)  // reserved
    layer            <- s2
    alternateGroup   <- s2
    volume           <- s2
    _                <- s2           // reserved
    matrix           <- s4.times(9)
    width            <- u4
    height           <- u4
  } yield TrackHeaderBox(version, flags, creationTime, modificationTime, trackId, duration, layer, alternateGroup, volume, matrix.toArray, width, height)

  private def trackReferenceTypeBoxHint (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[TrackReferenceTypeBoxHint] = for {
    trackIds <- u4.until(initialPosition + size)
  } yield TrackReferenceTypeBoxHint(trackIds)

  private def trackReferenceTypeBoxCdsc (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[TrackReferenceTypeBoxCdsc] = for {
    trackIds <- u4.until(initialPosition + size)
  } yield TrackReferenceTypeBoxCdsc(trackIds)

  private def mediaHeaderBoxBody: ByteParser[MediaHeaderBox] = for {
    (version, _)     <- fullBoxHeader
    creationTime     <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    modificationTime <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    timeScale        <- u4
    duration         <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    language         <- s2
    _                <- s2           // pre_defined
  } yield MediaHeaderBox(version, creationTime, modificationTime, timeScale, duration, language)

  private def handlerBoxBody: ByteParser[HandlerBox] = for {
    (version, _) <- fullBoxHeader
    _            <- u4               // pre_defined
    handlerType  <- u4
    _            <- u4.times(3)      // reserved
    name         <- nullEndedString
  } yield HandlerBox(version, handlerType, name)

  private def videoMediaHeaderBoxBody: ByteParser[VideoMediaHeaderBox] = for {
    (version, _) <- fullBoxHeader
    graphicsMode <- u2
    opColor      <- u2.times(3)
  } yield VideoMediaHeaderBox(version, graphicsMode, opColor.toArray)

  private def soundMediaHeaderBoxBody: ByteParser[SoundMediaHeaderBox] = for {
    (version, _) <- fullBoxHeader
    balance      <- s2
    _            <- u2   // reserved
  } yield SoundMediaHeaderBox(version, balance)

  private def hintMediaHeaderBoxBody: ByteParser[HintMediaHeaderBox] = for {
    (version, _) <- fullBoxHeader
    maxPDUSize   <- u2
    avgPDUSize   <- u2
    maxBitRate   <- u4
    avgBitRate   <- u4
    _            <- u4  // reserved
  } yield HintMediaHeaderBox(version, maxPDUSize, avgPDUSize, maxBitRate, avgBitRate)

  private def nullMediaHeaderBoxBody: ByteParser[NullMediaHeaderBox] = {
    for ((version, flags) <- fullBoxHeader) yield NullMediaHeaderBox(version, flags)
  }

  private lazy val selfContainedFlag = Unsigned(0x000001)

  private def dataReferenceBoxBody: ByteParser[DataReferenceBox] = for {
    (version, _) <- fullBoxHeader
    numEntries   <- u4
    entries      <- dataEntryBox.timesU(numEntries)
  } yield DataReferenceBox(version, entries)

  private def dataEntryBox: ByteParser[DataEntryBox] = makeBoxParser {
    case (_, _, Unsigned(0x75726c20)) => // 'url '
      dataEntryUrlBoxBody
    case (initialPosition, size, Unsigned(0x75726e20)) => // 'urn '
      dataEntryUrnBoxBody(initialPosition, size)
    case _ =>
      throw new RuntimeException("fail to parse");
  }

  private def dataEntryUrlBoxBody: ByteParser[DataEntryUrlBox] = for {
    (version, flags) <- fullBoxHeader
    locationOpt      <- if ((flags & selfContainedFlag) == 0) nullEndedString.map(Some(_)) else pure(None)
  } yield DataEntryUrlBox(version, flags, locationOpt)

  private def dataEntryUrnBoxBody (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[DataEntryUrnBox] = for {
    (version, flags) <- fullBoxHeader
    name             <- nullEndedString
    pos              <- currentPosition
    locationOpt      <- if (pos < initialPosition + size) nullEndedString.map(Some(_)) else pure(None)
  } yield DataEntryUrnBox(version, flags, name, locationOpt)

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
