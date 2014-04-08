package scanRenamer

import Parsing.Raw
import FileIO._
import scalaz._
import scalaz.syntax.semigroup._
import scalaz.syntax.foldable._
import scalaz.effect.IO._
import scalaz.std.list._
import scalaz.std.stream._
import scalaz.std.anyVal.unitInstance

import atto._, Atto._
import java.io.File
import scalaz.effect.IO
import scala.util.Try

case class LessonNumber(lesson: Int, page: Int)

object Parsing {
  type Raw = Int

  def imageNumber(name: String): Option[Raw] =
    Try(name.dropWhile(!_.isDigit).takeWhile(_.isDigit).toInt).toOption
  /*(many(elem(!_.isDigit)) ~> int).parseOnly(Algebra0021.jpg).option
  (string("Image (") ~> int <~ string(").jpg")).parse(name).option*/

  def lessonNumber(lesson: String): Option[LessonNumber] =
    (for {
      lesson <- int <~ char('.')
      page <- int <~ string(".jpg")
    } yield LessonNumber(lesson, page)).parse(lesson).option
}

object ScanRenamer {
  def rawOrLesson(name: String): Option[Raw \/ LessonNumber] = (Parsing.imageNumber(name), Parsing.lessonNumber(name)) match {
    case (_, Some(lesson)) => Some(\/-(lesson))
    case (Some(raw), _) => Some(-\/(raw))
    case _ => None
  }

  def categorize(files: List[File]): (List[(File, Raw)], List[(File, LessonNumber)]) = {
    val fileClassification: List[(File, Raw \/ LessonNumber)] = files.zip(files.map(file => rawOrLesson(name(file)))).collect {
      case (file, Some(rawOrLesson)) => (file, rawOrLesson)
    }
    val rawFiles = fileClassification.collect {
      case (file, -\/(raw)) => (file, raw)
    }
    val lessonFiles = fileClassification.collect {
      case (file, \/-(lesson)) => (file, lesson)
    }
    (rawFiles, lessonFiles)
  }

  def maxLesson(lessons: List[LessonNumber]): Raw = lessons.map(_.lesson).max

  def liftRawToLesson(raw: List[Raw], lessonNumber: Int): List[LessonNumber] = raw.sorted.zipWithIndex.map {
    case (_, index) => LessonNumber(lessonNumber, index + 1)
  }

  def lessonToFileName(lesson: LessonNumber) = s"${lesson.lesson}.${lesson.page}.jpg"

  def renameList(raw: List[(File, Raw)], lessons: List[LessonNumber]): List[(File, File)] = {
    val maximalLesson = maxLesson(lessons)
    val (files, raws) = raw.unzip
    files match {
      case Nil => Nil
      case sib :: _ => files zip liftRawToLesson(raws, maximalLesson + 1).map(lessonToFileName).map(sibling(sib, _))
    }
  }

  def renameAll(toRename: List[(File, File)]): IO[Unit] = toRename.foldMap {
    case (from, to) => renameFile(from, to).map(_ => ())
  }

  def renameFromListOfFiles(filesInFolder: List[File]): IO[Unit] = {
    val (raws, lessons) = categorize(filesInFolder)
    val renameFilesPairs = renameList(raws, lessons.map(_._2))
    renameAll(renameFilesPairs)
  }

  def renameFilesInFolder(folder: File): IO[Unit] =
    listFolder(folder).flatMap {
      case Some(fileList) => renameFromListOfFiles(fileList)
      case None => IO(())
    }

  def renameFilesInFolders(folders: Stream[File]): IO[Unit] = folders.foldMap(renameFilesInFolder)

  def getFoldersFromFileLines(index: File): IO[Stream[File]] = readFileLines(index).map(_.map(new File(_)))

  def renameFilesFromFoldersInIndex(index: File): IO[Unit] = getFoldersFromFileLines(index).flatMap(renameFilesInFolders)
}

object Main extends App {
  /*
  println("Please enter the folder to operate on: ")
  val folderString = readLine()
  val folder = new File(folderString)
  ScanRenamer.renameFilesInFolder(folder).unsafePerformIO()
  */

  val index = new File("folders.index")
  ScanRenamer.renameFilesFromFoldersInIndex(index).unsafePerformIO()
}
