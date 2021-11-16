package io.aakre.itv_mars_rover

import io.aakre.itv_mars_rover.Rover._

import scala.annotation.tailrec
import scala.util.Random


object RoverControl {

  def main(args: Array[String]): Unit = {

    val height = 5
    val width = 40
    val roughness = 0.05
    val roverStart = Coordinate(height/2, width/2)

    val rover = Rover(roverStart, North)
    val world = World.generate(width,height, roughness).insertRover(rover)

    loop(world)

    @tailrec def loop(w: World): World = {
      world.printMap()
      print("command: ")
      val cmd = parseCommand(scala.io.StdIn.readLine())
      if(cmd.isEmpty) {
        printHelp()
        loop(w)
      }
      else loop(w.execute(cmd.get))
    }
  }

  private def parseCommand(s: String): Option[Command] = {
    val command = s.toLowerCase match {
      case "w" => Some(Forward)
      case "a" => Some(AntiClockwise)
      case "d" => Some(Clockwise)
      case _ => None
    }
    if(s.toLowerCase == "q") System.exit(0)
    command
  }

  private def printHelp() = {
    println("Rover command 7000")
    println("==================")
    println("w - move forward")
    println("a - rotate anticlockwise")
    println("d - rotate clockwise")
    println("q - quit")
  }
}

case class World(width: Int, height: Int, map: Map[Coordinate, Cell]) { self =>

  def execute(cmd: Command): World = {
    println(cmd)
    cmd match {
      case Clockwise => getRover.fold(self)(r => self.updateRover(r.rotate(Clockwise)))
      case AntiClockwise => getRover.fold(self)(r => self.updateRover(r.rotate(AntiClockwise)))
      case Forward => getRover.fold(self)(r => self.updateRover(r.move()))
    }
  }

  def insertRover(rover: Rover): World = {
    val spotOpt = map.get(rover.location).map(c => c.copy(rover = Some(rover)))
    spotOpt.map(spot => self.copy(map = map.updated(rover.location, spot))).getOrElse(self)
  }

  def getRoverLocation: Option[Cell] = map.find(p => p._2.rover.isDefined).map(_._2) // super efficient
  def getRover: Option[Rover] = getRoverLocation.flatMap(_.rover)

  def updateRover(rover: Rover): World = {
    val oldOpt = getRoverLocation
    oldOpt.fold(self)(old => self.copy(map = map.updated(old.location, old.copy(rover = Some(rover)))))
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
      case East => "\uD83E\uDC46"
      case West => "\uD83E\uDC44"
    }
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
