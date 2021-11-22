package io.aakre.itv_mars_rover

import io.aakre.itv_mars_rover.Rover._

import scala.util.Random

case class World(width: Int, height: Int, map: Map[Coordinate, Cell], rover: Rover) { self =>

  def execute(cmd: Command): World = {
    cmd match {
      case Clockwise     => self.copy(rover = rover.rotate(Clockwise))
      case AntiClockwise => self.copy(rover = rover.rotate(AntiClockwise))
      case Forward       => self.moveRover(rover.move())
    }
  }

  def getRoverLocation: Option[Cell] = map.get(rover.location)

  def getCell(coordinate: Coordinate): Option[Cell] = self.map.get(coordinate)

  def moveRover(updatedRover: Rover): World = {
    val oldLoc = getRoverLocation
    if (self.getCell(updatedRover.location).fold(false)(c => c.isFlat)) {
      oldLoc.fold(self)(oldCell => self.roverExited(oldCell, updatedRover))
    } else self
  }

  def roverExited(oldCell: Cell, updatedRover: Rover): World =
    self.copy(map = map.updated(oldCell.location, oldCell.copy(feature = Flat(true))), rover = updatedRover)


  def printMap(): Unit = {
    val line = () => println("-" * width)

    line()
    cordspace.foreach { c =>
      if(c.x > 0 && c.y % width == 0) println()
      map.get(c).foreach(ce => print(ce.draw(self.rover)))
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
  def generate(width: Int, height: Int, roughness: Double, rover: Rover, seed: Long = Random.nextLong()): World = {
    if(width < 5 || height < 5) throw new IllegalArgumentException("World width and height must be greater than 5")
    else {
      val prng = new Random(seed)
      val d = (0 until height).flatMap { x =>
        (0 until width).map { y =>
          val terrain = if (prng.nextDouble() > roughness) Flat(false) else Mountain
          Cell(Coordinate(x, y), terrain)
        }
      }.map(cell => cell.location -> cell).toMap

      World(width, height, d, rover)
    }
  }
}

case class Coordinate(x: Int, y: Int)

case class Cell(location: Coordinate, feature: TerrainFeature) {

  def draw(rover: Rover) =
  if(rover.location == location)
    rover.direction match {
      case North => "\uD83E\uDC45"
      case South => "\uD83E\uDC47"
      case East  => "\uD83E\uDC46"
      case West  => "\uD83E\uDC44"
    }
  else feature.toString

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