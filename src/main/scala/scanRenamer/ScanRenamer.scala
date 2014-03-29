package scanRenamer

import FileIO._
import scalaz._
import scalaz.syntax.semigroup._
import scalaz.syntax.foldable._
import scalaz.effect.IO._
import scalaz.std.list._
import scalaz.std.anyVal.unitInstance

import atto._, Atto._
import java.io.File
import scalaz.effect.IO

case class LessonNumber(lesson: Int, page: Int)

object LessonNumber {
  val lesson: LessonNumber => Int = _.lesson
  val page: LessonNumber => Int = _.page
}

object Parsing {
  def imageNumber(name: String): Option[Int] =
    (string("Image (") ~> int <~ string(").jpg")).parse(name).option

  def lessonNumber(lesson: String): Option[LessonNumber] =
    (for {
      lesson <- int <~ char('.')
      page <- int <~ string(".jpg")
    } yield LessonNumber(lesson, page)).parse(lesson).option
}

object ScanRenamer {
  def rawOrLesson(name: String): Option[Int \/ LessonNumber] = (Parsing.imageNumber(name), Parsing.lessonNumber(name)) match {
    case (Some(raw), _) => Some(-\/(raw))
    case (_, Some(lesson)) => Some(\/-(lesson))
    case _ => None
  }

  def rawAndLessons(files: List[File]): (List[(File, Int)], List[(File, LessonNumber)]) = {
    val fileClassification: List[(File, Int \/ LessonNumber)] = files.zip(files.map(file => rawOrLesson(name(file)))).collect {
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

  def maxLesson(lessons: List[LessonNumber]): Int = lessons.map(LessonNumber.lesson).max

  def liftRawToLesson(raw: List[Int], lessonNumber: Int): List[LessonNumber] = raw.sorted.zipWithIndex.map {
    case (_, index) => LessonNumber(lessonNumber, index + 1)
  }

  def lessonToFileName(lesson: LessonNumber) = s"${lesson.lesson}.${lesson.page}.jpg"

  def renameList(raw: List[(File, Int)], lessons: List[LessonNumber]): List[(File, File)] = {
    val maximalLesson = maxLesson(lessons)
    val (files, raws) = raw.unzip
    val sib = files.head
    files zip liftRawToLesson(raws, maximalLesson + 1).map(lessonToFileName).map(sibling(sib, _))
  }

  def renameAll(toRename: List[(File, File)]): IO[Unit] = toRename.foldMap {
    case (from, to) => renameFile(from, to).map(_ => ())
  }

  def renameFromListOfFiles(filesInFolder: List[File]): IO[Unit] = {
    val (raws, lessons) = rawAndLessons(filesInFolder)
    val renameFilesPairs = renameList(raws, lessons.map(_._2))
    renameAll(renameFilesPairs)
  }

  def magic(folder: File): IO[Unit] =
    listFolder(folder).flatMap {
      case Some(fileList) => renameFromListOfFiles(fileList)
      case None => IO(())
    }
}

object Main extends App {
  println("Please enter the folder to operate on: ")
  val folderString = readLine()
  val folder = new File(folderString)
  ScanRenamer.magic(folder).unsafePerformIO()
}
