package io.aakre.tiny_web_crawler

import scala.util.Random

object RoverControl {


  def main(args: Array[String]): Unit = {
    val height = 5
    val width = 40
    val roughness = 0.05
    val roverStart = Coordinate(height/2, width/2)

    val rover = Rover(roverStart)
    val world = World.generate(width,height, roughness).map(_.insertRover(rover))

    println("Rover command 7000")
    println("==================")
    world.foreach(_.printMap())
  }
}

case class World(width: Int, height: Int, map: Map[Coordinate, Cell]) { self =>

  def insertRover(rover: Rover): World = {
    val spotOpt = map.get(rover.location).map(c => c.copy(rover = Some(rover)))
    spotOpt.map(spot => self.copy(map = map.updated(rover.location, spot))).getOrElse(self)
  }

  def printMap(): Unit = {
    val line = () => println("-" * ((width)))

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
  def generate(width: Int, height: Int, roughness: Double, seed: Long = Random.nextLong()): Either[String,World] = {
    if(width < 5 || height < 5) Left ("World width and height must be greater than 5")
    else {
      val prng = new Random(seed)
      val d = (0 until height).flatMap { x =>
        (0 until width).map { y =>
          val terrain = if (prng.nextDouble() > roughness) Flat(false) else Mountain
          Cell(Coordinate(x, y), terrain, None)
        }
      }.map(cell => cell.location -> cell).toMap

      Right(World(width, height, d))
    }
  }
}

case class Coordinate(x: Int, y: Int)

case class Cell(location: Coordinate, feature: TerrainFeature, rover: Option[Rover]) {
  override def toString: String = rover.fold(feature.toString)(_ => "#")
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
