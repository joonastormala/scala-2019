package o1.robots.gui

import o1.robots.tribal._
import o1.util._
import o1.gui.Dialog._
import scala.collection.immutable.SortedMap
import scala.util.control.NonFatal
import o1.util.localURL
import java.nio.file.Paths

////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////


/** The singleton object `TribeLoader` is capable of checking what robot
  * tribes are available in a particular folder, and loading the ones that are.
  *
  * '''NOTE TO STUDENTS: In this course, you don't need to understand how this object
  * works or can be used.''' */
object TribeLoader {

  val Suffix = ".tribe"

  val All = loadTribes
  val BunnyTribe = All.values.flatten.find( _.name == "bunny" )

  type TribeMap = SortedMap[Path, Option[Tribe]]

  private def loadTribes: TribeMap = {
    this.tribesDir.flatMap(loadTribeDir).getOrElse(SortedMap())
  }

  private def tribesDir = {
    val name = "tribes/"
    val folderUnderWorkingDir = Path(name)
    if (folderUnderWorkingDir.isReadable) {
      Some(folderUnderWorkingDir)
    } else {
      val folderUnderClassDir = localURL(name).map( url => Paths.get(url.toURI) )
      folderUnderClassDir.filter( _.isReadable )
    }
  }

  private def loadTribeDir(dir: Path): Option[TribeMap] = {
    def isTribeFile(candidate: Path) = candidate.fileName.endsWith(Suffix) && candidate.fileName.length > Suffix.length
    val dateOrder = Ordering.by( (p:Path) => p.lastModified ).reverse
    dir.listFiles(isTribeFile).map( tribeFiles => {
      val sortedTribes = tribeFiles.sorted(dateOrder)
      val nameTribePairs = for (file <- tribeFiles) yield file -> this.readTribe(file)
      SortedMap(nameTribePairs:_*)(Ordering.by(sortedTribes.indexOf))
    })
  }


  private def readTribe(path: Path): Option[Tribe] = {
    val tribeName = path.fileName.dropRight(Suffix.length)
    Try(new Tribe(tribeName, path.toString)) match {
      case Failure(roboSpeakProblem: Tribe.TribeFileException) =>
        display("Problem in the RoboSpeak file " + path.fileName + ":\n" + roboSpeakProblem.getMessage, Centered)
        System.err.println(roboSpeakProblem)
        None
      case Failure(NonFatal(otherProblem)) =>
        System.err.println(otherProblem)
        throw otherProblem
      case Success(tribe) =>
        Some(tribe)
    }
  }


}

