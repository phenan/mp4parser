package com.phenan.mp4

import java.io.File
import com.phenan.util._

import scala.util._

object Mp4Parsers extends ByteParsers {
  lazy val all: ByteParser[List[Box]] = box.untilEoF

  lazy val box: ByteParser[Box] = makeBoxParser(boxBody)

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

  private lazy val fullBoxHeader: ByteParser[(UnsignedByte, UnsignedInt)] = for {
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
    case 0x7374626c =>  // 'stbl'
      pure(SampleTableBox())
    case 0x73747473 =>  // 'stts'
      timeToSampleBoxBody
    case 0x63747473 =>  // 'ctts'
      compositionOffsetBoxBody
    case 0x73747364 =>  // 'stsd'
      sampleDescriptionBoxBody
    case 0x7374737a =>  // 'stsz'
      sampleSizeBoxBody
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

  private lazy val movieHeaderBoxBody: ByteParser[MovieHeaderBox] = for {
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

  private lazy val trackHeaderBoxBody: ByteParser[TrackHeaderBox] = for {
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

  private lazy val mediaHeaderBoxBody: ByteParser[MediaHeaderBox] = for {
    (version, _)     <- fullBoxHeader
    creationTime     <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    modificationTime <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    timeScale        <- u4
    duration         <- if (version == 1) u8 else u4.map(_.toUnsignedLong)
    language         <- s2
    _                <- s2           // pre_defined
  } yield MediaHeaderBox(version, creationTime, modificationTime, timeScale, duration, language)

  private lazy val handlerBoxBody: ByteParser[HandlerBox] = for {
    (version, _) <- fullBoxHeader
    _            <- u4               // pre_defined
    handlerType  <- u4
    _            <- u4.times(3)      // reserved
    name         <- nullEndedString
  } yield HandlerBox(version, handlerType, name)

  private lazy val videoMediaHeaderBoxBody: ByteParser[VideoMediaHeaderBox] = for {
    (version, _) <- fullBoxHeader
    graphicsMode <- u2
    opColor      <- u2.times(3)
  } yield VideoMediaHeaderBox(version, graphicsMode, opColor.toArray)

  private lazy val soundMediaHeaderBoxBody: ByteParser[SoundMediaHeaderBox] = for {
    (version, _) <- fullBoxHeader
    balance      <- s2
    _            <- u2   // reserved
  } yield SoundMediaHeaderBox(version, balance)

  private lazy val hintMediaHeaderBoxBody: ByteParser[HintMediaHeaderBox] = for {
    (version, _) <- fullBoxHeader
    maxPDUSize   <- u2
    avgPDUSize   <- u2
    maxBitRate   <- u4
    avgBitRate   <- u4
    _            <- u4  // reserved
  } yield HintMediaHeaderBox(version, maxPDUSize, avgPDUSize, maxBitRate, avgBitRate)

  private lazy val nullMediaHeaderBoxBody: ByteParser[NullMediaHeaderBox] = for {
    (version, flags) <- fullBoxHeader
  } yield NullMediaHeaderBox(version, flags)

  private lazy val selfContainedFlag = Unsigned(0x000001)

  private lazy val dataReferenceBoxBody: ByteParser[DataReferenceBox] = for {
    (version, _) <- fullBoxHeader
    numEntries   <- u4
    entries      <- dataEntryBox.timesU(numEntries)
  } yield DataReferenceBox(version, entries)

  private lazy val dataEntryBox: ByteParser[DataEntryBox] = makeBoxParser {
    case (_, _, Unsigned(0x75726c20)) => // 'url '
      dataEntryUrlBoxBody
    case (initialPosition, size, Unsigned(0x75726e20)) => // 'urn '
      dataEntryUrnBoxBody(initialPosition, size)
    case _ =>
      throw new RuntimeException("fail to parse");
  }

  private lazy val dataEntryUrlBoxBody: ByteParser[DataEntryUrlBox] = for {
    (version, flags) <- fullBoxHeader
    locationOpt      <- if ((flags & selfContainedFlag) == 0) nullEndedString.map(Some(_)) else pure(None)
  } yield DataEntryUrlBox(version, flags, locationOpt)

  private def dataEntryUrnBoxBody (initialPosition: UnsignedLong, size: UnsignedLong): ByteParser[DataEntryUrnBox] = for {
    (version, flags) <- fullBoxHeader
    name             <- nullEndedString
    pos              <- currentPosition
    locationOpt      <- if (pos < initialPosition + size) nullEndedString.map(Some(_)) else pure(None)
  } yield DataEntryUrnBox(version, flags, name, locationOpt)

  private lazy val timeToSampleBoxBody: ByteParser[TimeToSampleBox] = for {
    (version, _) <- fullBoxHeader
    numEntries   <- u4
    entries      <- timeToSampleEntry.timesU(numEntries)
  } yield TimeToSampleBox(version, entries)

  private lazy val timeToSampleEntry: ByteParser[TimeToSampleEntry] = for {
    sampleCount <- u4
    sampleDelta <- u4
  } yield TimeToSampleEntry(sampleCount, sampleDelta)

  private lazy val compositionOffsetBoxBody: ByteParser[CompositionOffsetBox] = for {
    (version, _) <- fullBoxHeader
    numEntries   <- u4
    entries      <- compositionOffsetEntry.timesU(numEntries)
  } yield CompositionOffsetBox(version, entries)

  private lazy val compositionOffsetEntry: ByteParser[CompositionOffsetEntry] = for {
    sampleCount  <- u4
    sampleOffset <- u4
  } yield CompositionOffsetEntry(sampleCount, sampleOffset)

  private lazy val sampleDescriptionBoxBody: ByteParser[SampleDescriptionBox] = for {
    (version, _) <- fullBoxHeader
    entryCount   <- u4
    entries      <- sampleEntry.timesU(entryCount)
  } yield SampleDescriptionBox(version, entries)

  private lazy val sampleEntry: ByteParser[SampleEntry] = makeBoxParser { (initialPosition, size, boxType) =>
    for {
      _                  <- u1.times(6)  // reserved
      dataReferenceIndex <- u2
      entry              <- sampleEntryBody(initialPosition, size, boxType, dataReferenceIndex)
    } yield entry
  }

  private def sampleEntryBody (initialPosition: UnsignedLong, size: UnsignedLong, boxType: UnsignedInt, dataReferenceIndex: UnsignedShort): ByteParser[SampleEntry] = boxType.underlying match {
    case 0x72747020     // 'rtp '
         | 0x73727470   // 'srtp'
    => hintSampleEntry(initialPosition, size, boxType, dataReferenceIndex)
    case 0x61766331     // 'avc1'
         | 0x61766332   // 'avc2'
         | 0x61766333   // 'avc3'
         | 0x61766334   // 'avc4'
         | 0x656e6376   // 'encv'
         | 0x6170636e   // 'apcn'
         | 0x6d703476   // 'mp4v'
         | 0x73323633   // 's263'
         | 0x64766865   // 'dvhe'
         | 0x64766176   // 'dvav'
         | 0x68657631   // 'hev1'
         | 0x68766331   // 'hvc1'
    => visualSampleEntry(boxType, dataReferenceIndex)
    case 0x73616d72     // 'samr'
         | 0x73617762   // 'sawb'
         | 0x6d703461   // 'mp4a'
         | 0x64726d73   // 'drms'
         | 0x6f776d61   // 'owma'
         | 0x61632d33   // 'ac-3'
         | 0x65632d33   // 'ec-3'
         | 0x656e6361   // 'enca'
         | 0x736f7774   // 'sowt'
    => audioSampleEntry(boxType, dataReferenceIndex)
    case _
    => unknownSampleEntry(initialPosition, size, boxType, dataReferenceIndex)
  }

  private def hintSampleEntry (initialPosition: UnsignedLong, size: UnsignedLong, boxType: UnsignedInt, dataReferenceIndex: UnsignedShort): ByteParser[HintSampleEntry] = for {
    data <- bytesUntil(initialPosition + size)
  } yield HintSampleEntry(dataReferenceIndex, boxType, data)

  private def visualSampleEntry (boxType: UnsignedInt, dataReferenceIndex: UnsignedShort): ByteParser[VisualSampleEntry] = for {
    _              <- u2           // pre_defined
    _              <- u2           // reserved
    _              <- u4.times(3)  // pre_defined
    width          <- u2
    height         <- u2
    hResolution    <- u4
    vResolution    <- u4
    _              <- u4           // reserved
    frameCount     <- u2
    nameLength     <- u1
    compressorName <- bytes(nameLength.toUnsignedLong).map(new String(_))
    _              <- bytes(Unsigned(31L) - nameLength.toUnsignedLong)     // padding
    depth          <- u2
    _              <- u2           // pre_defined
  } yield VisualSampleEntry(dataReferenceIndex, boxType, width, height, hResolution, vResolution, frameCount, compressorName, depth)

  private def audioSampleEntry (boxType: UnsignedInt, dataReferenceIndex: UnsignedShort): ByteParser[AudioSampleEntry] = for {
    _            <- u4.times(2)    // reserved
    channelCount <- u2
    sampleSize   <- u2
    _            <- u2             // pre_defined
    _            <- u2             // reserved
    sampleRate   <- u4
  } yield AudioSampleEntry(dataReferenceIndex, boxType, channelCount, sampleSize, sampleRate)

  private def unknownSampleEntry (initialPosition: UnsignedLong, size: UnsignedLong, boxType: UnsignedInt, dataReferenceIndex: UnsignedShort): ByteParser[UnknownSampleEntry] = for {
    data <- bytesUntil(initialPosition + size)
  } yield UnknownSampleEntry(dataReferenceIndex, boxType, data)

  private lazy val sampleSizeBoxBody: ByteParser[SampleSizeBox] = for {
    (version, _) <- fullBoxHeader
    sampleSize   <- u4
    sampleCount  <- u4
    entries      <- if (sampleSize == 0) u4.timesU(sampleCount) else pure(Nil)
  } yield SampleSizeBox(version, sampleSize, sampleCount, entries)

  private def unknownBox (initialPosition: UnsignedLong, size: UnsignedLong, boxType: UnsignedInt): ByteParser[UnknownBox] = for {
    data <- bytesUntil(initialPosition + size)
  } yield UnknownBox(boxType, data)

  def main (args: Array[String]): Unit = {
    all.parse(new File("test.mp4")) match {
      case Success(v) => println(v.map(_.toPrettyString).mkString("\n"))
      case Failure(e) => e.printStackTrace()
    }
  }
}
