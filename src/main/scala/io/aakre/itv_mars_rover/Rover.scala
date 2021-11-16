package io.aakre.itv_mars_rover

import io.aakre.itv_mars_rover.Rover._

case class Rover(location: Coordinate, direction: Direction) { self =>
  def move(): Rover = {
    val newLocation = direction match {
      case North => location.copy(x = location.x-1)
      case South => location.copy(x = location.x+1)
      case East  => location.copy(y = location.y+1)
      case West  => location.copy(y = location.y-1)
    }
    self.copy(location = newLocation)
  }

  def rotate(r: Rotation): Rover = {
    val newDirection = r match {
      case Clockwise =>
        self.direction match {
          case North => East
          case East  => South
          case South => West
          case West  => North
        }

      case AntiClockwise =>
        self.direction match {
          case North => West
          case East  => North
          case South => East
          case West  => South
        }
    }
    self.copy(direction = newDirection)
  }

  override def toString: String = s"Rover at $location, facing $direction"
}

object Rover {
  sealed trait Rotation

  sealed trait Command {
    override def toString: String = "???"
  }
  final case object Forward extends Command {
    override def toString: String = "Moving forward"
  }
  final case object Clockwise extends Command with Rotation {
    override def toString: String = "Turning clockwise"
  }
  final case object AntiClockwise extends Command with Rotation {
    override def toString: String = "turning anticlockwise"
  }

  sealed trait Direction
  final case object North extends Direction
  final case object South extends Direction
  final case object East extends Direction
  final case object West extends Direction
}