package io.aakre.itv_mars_rover

import io.aakre.itv_mars_rover.Rover._

import scala.util.Random

case class World(width: Int, height: Int, map: Map[Coordinate, Cell]) { self =>

  def execute(cmd: Command): World = {
    println(self.getRover.getOrElse("ERROR, NO ROVER") + " > " + cmd)
    cmd match {
      case Clockwise     => getRover.fold(self)(r => self.updateRover(r.rotate(Clockwise)))
      case AntiClockwise => getRover.fold(self)(r => self.updateRover(r.rotate(AntiClockwise)))
      case Forward       => getRover.fold(self)(r => self.updateRover(r.move()))
    }
  }

  def insertRover(rover: Rover): World = {
    val spotOpt = map.get(rover.location).map(c => c.copy(rover = Some(rover)))
    spotOpt.map(spot => self.copy(map = map.updated(rover.location, spot))).getOrElse(self)
  }

  def getRoverLocation: Option[Cell] = map.find(p => p._2.rover.isDefined).map(_._2) // super efficient
  def getRover: Option[Rover] = getRoverLocation.flatMap(_.rover)

  def getCell(coordinate: Coordinate) = self.map.get(coordinate)

  def updateRover(rover: Rover): World = {
    val oldLoc = getRoverLocation
    // rotate rover
    val rotApplied = oldLoc.fold(self)(oldRover =>
      self.copy(map = map.updated(oldRover.location, oldRover.copy(rover = Some(rover))))
    )
    // move rover if rover.location updated
    if(oldLoc.fold(false)(_.location != rover.location)) {
      println("MOVING ROVER!") // TODO delete
      if(rotApplied.getCell(rover.location).fold(false)(c => c.isFlat)) {
        println("FLAT AHEAD!)")
        val left = oldLoc.fold(rotApplied)(oldCell => self.roverExited(oldCell))
        left.getCell(rover.location).fold(left)(left.roverEntered(_, rover))
      }
      else {
        println("STOPPED BY TERRAIN") // todo delete
        rotApplied
      }
    } else rotApplied
  }

  def roverExited(oldCell: Cell): World =
    self.copy(map = map.updated(oldCell.location, oldCell.copy(feature = Flat(true), rover = None)))

  def roverEntered(newCell: Cell, rover: Rover): World = {
    self.copy(map = map.updated(newCell.location, newCell.copy(rover = Some(rover))))
  }

  def printMap(): Unit = {
    val line = () => println("-" * width)

    line()
    cordspace.foreach { c =>
      if(c.x > 0 && c.y % width == 0) println()
      map.get(c).foreach(print)
    }
    println()
    line()
  }

  private val cordspace =
    for {
      x <- 0 until height
      y <- 0 until width
    } yield Coordinate(x,y)
}

object World {
  def generate(width: Int, height: Int, roughness: Double, seed: Long = Random.nextLong()): World = {
    if(width < 5 || height < 5) throw new IllegalArgumentException("World width and height must be greater than 5")
    else {
      val prng = new Random(seed)
      val d = (0 until height).flatMap { x =>
        (0 until width).map { y =>
          val terrain = if (prng.nextDouble() > roughness) Flat(false) else Mountain
          Cell(Coordinate(x, y), terrain, None)
        }
      }.map(cell => cell.location -> cell).toMap

      World(width, height, d)
    }
  }
}

case class Coordinate(x: Int, y: Int)

case class Cell(location: Coordinate, feature: TerrainFeature, rover: Option[Rover]) {
  override def toString: String = rover.fold(feature.toString) { r =>
    r.direction match {
      case North => "\uD83E\uDC45"
      case South => "\uD83E\uDC47"
      case East  => "\uD83E\uDC46"
      case West  => "\uD83E\uDC44"
    }
  }

  def isFlat: Boolean = feature match {
    case Flat(_) => true
    case _ => false
  }
}

sealed trait TerrainFeature {
  override def toString: String = "?"
}
case object Mountain extends TerrainFeature {
  override def toString: String = "^"
}
case class Flat(tracks: Boolean) extends TerrainFeature {
  override def toString: String = if(tracks) "." else " "
}