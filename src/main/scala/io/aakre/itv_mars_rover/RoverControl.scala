package io.aakre.itv_mars_rover

import io.aakre.itv_mars_rover.Rover._

import scala.annotation.tailrec

object RoverControl {

  def main(args: Array[String]): Unit = {
    val height = 5
    val width = 40
    val roughness = 0.15
    val roverStart = Coordinate(height/2, width/2)

    val rover = Rover(roverStart, North)
    loop(World.generate(width,height, roughness).insertRover(rover))

    @tailrec def loop(w: World): World = {
      w.printMap()
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
      case _   => None
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