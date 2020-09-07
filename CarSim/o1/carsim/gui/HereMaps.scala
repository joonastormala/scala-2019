////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////

package o1.carsim.gui

import org.openstreetmap.gui.jmapviewer.tilesources.AbstractOsmTileSource
import org.openstreetmap.gui.jmapviewer.interfaces.TileSource.TileUpdate
import scala.util._
import rapture._
import net._
import json._, jsonBackends.jackson._
import io._
import uri._
import codec._, encodings.`UTF-8`._
import core._
import modes.throwExceptions._
import modes.returnTry._
import timeSystems.javaUtil
import org.openstreetmap.gui.jmapviewer.interfaces.TileCache
import org.openstreetmap.gui.jmapviewer.Tile
import org.openstreetmap.gui.jmapviewer.interfaces.TileSource
import java.util.zip.ZipOutputStream
import java.util.zip.ZipEntry
import javax.imageio.ImageIO

/** The singleton object `HereMaps` provides an interface to the Here.com routing and map service over the internet.
  *
  * '''NOTE TO STUDENTS: In this course, you don't need to understand how this object works or can be used.'''
  *
  * (This class is not really a part of the GUI ''per se''. It is here as an artifact of the programming assigment.) */
object HereMaps { // See: https://developer.here.com/documentation

  def findRoute(origin: String, destination: String) = {

    implicit val StepExtractor = Json.extractor[Json].map { json =>
      Step(json.shape.as[Array[String]],
        json.summary.as[Json])
    }

    def scrapeRouteSegments(directionsJson: Json) = {
      attempt(directionsJson.response.route(0).as[Step].segments, "Failed to get the route. Received an invalid JSON response from HERE Maps.")
    }

    val originCoords          = parseCoords(origin)      getOrElse error("Invalid origin of travel: " + origin)
    val destinationCoords     = parseCoords(destination) getOrElse coordsOfNamedPlace(destination, maybeNear=originCoords)
    val searchParameters      = Map("app_id" -> APIKeys.AppId, "app_code" -> APIKeys.AppCode, "mode" -> "fastest;car;traffic:disabled", "representation" -> "overview",
                                    "routeattributes" -> "shape", "waypoint0" -> originCoords.toString, "waypoint1" -> destinationCoords.toString)
    val searchURL: HttpUrl    = uri"https://route.api.here.com/routing/7.2/calculateroute.json"
    val query: HttpQuery      = attempt(searchURL.query(searchParameters), "Failed to access HERE Maps route search. Please make sure your network connection is working.")
    val responseBody: String  = attempt(query.slurp[Char], "No route found.")
    val directionsJson        = attempt(Json.parse(responseBody), "Received an invalid JSON response from the HERE Maps route service.")
    scrapeRouteSegments(directionsJson)

  }

  case class Coords(val lat: Double, val lng: Double) {
    override def toString = s"geo!${lat},${lng}"
  }

  case class Segment(val distance: Double, val origin: Coords, val destination: Coords)

  case class Step(val shape: Array[String], val summary: Json) {
    lazy val segments = {
      def distanceSortOf(a: Coords, b: Coords) = math.hypot(b.lat - a.lat, b.lng - a.lng)
      val points         = shape.map(x => convertToCoords(x))
      val lengthsAndEnds = for ((from, to) <- points zip points.tail) yield (distanceSortOf(from, to), from, to)
      val totalLength    = lengthsAndEnds.foldLeft(0.0)( _ + _._1 )
      for ((dist, from, to) <- lengthsAndEnds) yield Segment(dist / totalLength * this.summary.distance.as[Int], from, to) // the length in meters for each polyline segment is not provided, so we use this simple metric that is good enough for current purposes
    }

    private def convertToCoords(coords: String) = {
      val parts = coords.split(",").map(_.toDouble)
      new Coords(parts(0), parts(1))
    }
  }


  def coordsOfNamedPlace(placeName: String, maybeNear: Coords) = {
    val searchParameters   = Map("app_id" -> APIKeys.AppId, "app_code" -> APIKeys.AppCode, "searchtext" -> placeName,
                                 "maxresults" -> "1", "prox" -> s"${maybeNear.lat},${maybeNear.lng},200000")
    val searchURL: HttpUrl = uri"https://geocoder.api.here.com/6.2/geocode.json"
    val query: HttpQuery      = searchURL.query(searchParameters)
    val responseBody       = attempt(query.slurp[Char], "Failed to access HERE Maps coordinate search. Please make sure your network connection is working.")
    val locationJson       = attempt(Json.parse(responseBody),                                        "Received an invalid JSON response from HERE Maps coordinate search.")
    val coords             = attempt(locationJson.Response.View(0).Result(0).Location.NavigationPosition(0).as[Json], s"No coordinates available for '$placeName'.")
    Coords(coords.Latitude.as[Double], coords.Longitude.as[Double])
  }

  private object APIKeys { // Please do not use these API keys in other projects. You can create your own for free at Here.com if you want.
    val AppId  = "683xUCoL3LPk49DlRwIM"
    val AppCode = "09umXyLDjeevTBdp_bm7ow"
  }

  private def error(message: String) = throw new CarMap.DirectionsException(message)
  private def attempt[Result](action: =>Result, message: String) = Try(action) getOrElse error(message)

  private val HttpOK = 200
  private val LatCommaLong = {
    val Comma = """\s*,\s*"""
    val Coord = """((?:-)?\d+(?:\.\d+)?)"""
    (Coord + Comma + Coord).r
  }
  private def parseCoords(coordsString: String) = coordsString match {
    case LatCommaLong(lat, lng) => Try(Coords(lat.toDouble, lng.toDouble)).toOption
    case _                      => None
  }

  class HereMapsTileSource extends AbstractOsmTileSource("HERE", "") {

    val servers = Array("1", "2", "3", "4")
    var server = 0

    override def getBaseUrl = {
      server = (server + 1) % servers.length
      s"https://${servers(server)}.base.maps.api.here.com/maptile/2.1/maptile/newest/normal.day"
    }

    override def getTileUrl(zoom: Int, tilex: Int, tiley: Int) = {
      this.getBaseUrl() + s"/${zoom}/${tilex}/${tiley}/256/png8?app_id=${APIKeys.AppId}&app_code=${APIKeys.AppCode}"
    }

    override def getTileUpdate = {
      TileUpdate.IfNoneMatch
    }

    override def getMaxZoom = 16

  }

}