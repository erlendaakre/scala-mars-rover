package io.aakre.quantexa

object Main {

  case class Transaction( transactionId: String, accountId: String, transactionDay: Int, category: String, transactionAmount: Double)

  def main(args: Array[String]): Unit = {
    val txs = readTx()

    println("1. Total transaction value per day:")
    txs.groupBy(_.transactionDay).foreach(t => println(s"${t._1} = ${t._2.map(_.transactionAmount).sum}"))

    println("2. Avg transaction value per account/type")
    def avg(l: List[Transaction]) = l.map(_.transactionAmount).sum / l.length
    txs.groupBy(_.accountId).foreach(t => println(s"${t._1} = ${t._2.groupBy(_.category).map(c => s"${c._1} (${avg(c._2)})")}"))
  }

  def readTx(): List[Transaction] = {
    import scala.io.Source
    val fileName = "transactions.txt"
    val transactionslines: Iterator[String] = Source.fromFile(fileName).getLines().drop(1)
    transactionslines.map { line =>
      val split = line.split(',')
      Transaction(split(0), split(1), split(2).toInt, split(3), split(4).toDouble)
    }.toList
  }
}
