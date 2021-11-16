package io.aakre.tiny_web_crawler

import scala.util.Random

object RoverControl {


  def main(args: Array[String]): Unit = {
    val height = 10
    val width = 30
    val roughness = 0.05
    val roverStart = Coordinate(height/2, width/2)

    val rover = Rover(roverStart)
    val world = World.generate(width,height, roughness).map(_.insertRover(rover))

    println("Rover command 7000")
    println("==================")
    world.foreach(_.printMap())
  }
}

case class World(width: Int, height: Int, map: Seq[Seq[Cell]]) { self =>

  def insertRover(rover: Rover): World = {
    self.copy(map = map.map { row =>
      if(row.head.location.x == rover.location.x) {
        row.map { cell =>
          if(cell.location.y == rover.location.y) cell.copy(rover = Some(rover)) else cell
        }
      }
      else row
    })
  }

  def printMap(): Unit = {
    val line = () => println("-" * ((width*2)-1))

    line()
    map.foreach(row =>
      println(row.mkString(" "))
    )
    line()
  }
}

object World {
  def generate(width: Int, height: Int, roughness: Double, seed: Long = Random.nextLong()): Either[String,World] = {
    if(width < 5 || height < 5) Left ("World width and height must be greater than 5")
    else {
      val prng = new Random(seed)
      val d = (0 until height).map { x =>
        (0 until width).map { y =>
          val terrain = if (prng.nextDouble() > roughness) Flat(false) else Mountain
          Cell(Coordinate(x, y), terrain, None)
        }
      }
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
