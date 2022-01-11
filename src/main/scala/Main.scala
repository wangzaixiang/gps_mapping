package wangzx.gpsutil

import java.io.File
import java.math.MathContext
import scala.io.Source

object Main {

  case class Point(lon: Double, lat: Double)

  /**
   * real: the real GPS coordinate
   * map: the map coordinate.
   */
  case class PointMapping(real: Point, map: Point)

  val LON_START = 73.40
  val LON_END = 135.40
  val LAT_START = 18.1
  val LAT_END = 53.6

  val mapTable: Array[Double] = buildMapTable()

  def buildMapTable(): Array[Double] ={
    val buffer = new Array[Double](441796 * 2)
    new File("crack").listFiles().foreach { f: File =>
      Source.fromFile(f).getLines().foreach { line =>
        val Array(lon, lat, lon2, lat2) = line.split(" ")
        val lonIdx = ((BigDecimal(lon).round(MathContext.DECIMAL32) - BigDecimal(LON_START))/ BigDecimal(0.05)).toInt
        val latIdx = ((BigDecimal(lat).round(MathContext.DECIMAL32) - BigDecimal(LAT_START))/ BigDecimal(0.1)).toInt
        val idx = (lonIdx * 356 + latIdx) * 2   // (53.6 - 18.1)/ 0.1 + 1
        assert(buffer(idx)==0 && buffer(idx+1)==0)
        buffer(idx) = lon2.toDouble
        buffer(idx+1) = lat2.toDouble
      }
    }
    buffer
  }

  def getMapPointFor(lon: Double, lat: Double): PointMapping = {
    val lonIdx = ((BigDecimal(lon).round(MathContext.DECIMAL32) - BigDecimal(LON_START))/ BigDecimal(0.05)).toInt
    val latIdx = ((BigDecimal(lat).round(MathContext.DECIMAL32) - BigDecimal(LAT_START))/ BigDecimal(0.1)).toInt
    val idx = (lonIdx * 356 + latIdx) * 2
    PointMapping(Point(lon, lat), Point(mapTable(idx), mapTable(idx+1)) )
  }

  def map(p: Point): Point = {

    val lon = (BigDecimal(p.lon).round(MathContext.DECIMAL32) / BigDecimal(0.05)).toInt * BigDecimal(0.05)
    val lat = (BigDecimal(p.lat).round(MathContext.DECIMAL32) / BigDecimal(0.1)).toInt * BigDecimal(0.1)

    val (x, y) = (lon.toDouble, lat.toDouble)
    val ws = getMapPointFor(x, y)
    val wn = getMapPointFor(x, y+0.1)
    val en = getMapPointFor(x+0.05, y+0.1)
    val es = getMapPointFor(x+0.05, y)

    map(p, ws, wn, en, es)
  }

  def main(args: Array[String]): Unit = {

    // 22.543218°N 113.900249°巨
//    val p = Point(113.883599, 22.504271)
    val p = Point(113.900249, 22.543218)
    val mapped = map(p)

    println(s"$p => $mapped")
  }

  /**
   * @param p the point in real world
   * @param ws west-south corner point
   * @param wn west-north corner point
   * @param en east-north corner point
   * @param es east-south corner point
   * @return the point in map
   */
  private def map(p: Point, ws: PointMapping, wn: PointMapping, en: PointMapping, es: PointMapping): Point = {
    val m_w = getPointAtRate( ws.map, wn.map, (p.lat - ws.real.lat) / (wn.real.lat - ws.real.lat))  // map point for west
    val m_n = getPointAtRate( wn.map, en.map, (p.lon - wn.real.lon) / (en.real.lon - wn.real.lon))  // map point for north
    val m_e = getPointAtRate( en.map, es.map, (p.lat - en.real.lat) / (es.real.lat - en.real.lat))  // map point for east
    val m_s = getPointAtRate( ws.map, es.map, (p.lon - ws.real.lon) / (es.real.lon - ws.real.lon))  // map point for south

    crossPoint(m_w, m_e, m_s, m_n)
  }

  private def getPointAtRate( src: Point, dest: Point, rate: Double): Point =
    Point(src.lon + (dest.lon - src.lon) * rate, src.lat + (dest.lat - src.lat)*rate)

  private def crossPoint(start1: Point, end1: Point, start2: Point, end2: Point): Point = {
    if(end1.lon == start1.lon) {
      val a2 = (end2.lat - start2.lat) / (end2.lon - start2.lon)
      val b2 = start2.lat - a2 * start2.lon
      val x = start1.lon
      val y = a2 * x + b2
      Point(x, y)
    }
    else if(end2.lon == start2.lon) {
      val a1 = (end1.lat - start1.lat) / (end1.lon - start1.lon)
      val b1 = start1.lat - a1 * start1.lon
      val x = start2.lon
      val y = a1 * x + b1
      Point(x, y)
    }
    else {
      // line1: y = ax + b
      val a1 = (end1.lat - start1.lat) / (end1.lon - start1.lon)
      val b1 = start1.lat - a1 * start1.lon

      // line2
      val a2 = (end2.lat - start2.lat) / (end2.lon - start2.lon)
      val b2 = start2.lat - a2 * start2.lon

      // a1x + b1 = a2x + b2
      val x = (b2 - b1) / (a1 - a2)
      val y = a1 * x + b1
      Point(x, y)
    }
  }

}
