package io.aakre.tiny_web_crawler

case class Rover(location: Coordinate) {
}

object Rover {
  sealed trait Command

  final case object Forward extends Command
  final case object Clockwise extends Command
  final case object AntiClockwise extends Command
}