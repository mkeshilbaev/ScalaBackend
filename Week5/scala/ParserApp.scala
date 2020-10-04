import java.io.{File, PrintWriter}
import scala.util.matching.Regex
import Patterns._
import scala.io.Source
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._
import scala.util.matching.Regex.Match


case class Product(name: String, quantity: Int, price: Double, sum: Double)

case class Receipt(orgTitle: String, orgId: String, kkmSerialNumber: String,
                   kkmId: String, fiscalId: String, placeAddress: String,
                   transactionDate: String, items: List[Product], totalSum: Double)

object Patterns {
  val NUMBER_PATTERN: Regex = "[1-9][0-9]\\.".r
  val MULTIPLICATION_PATTERN: Regex = "([0-9] ?[0-9]+\\.[0-9]+) x ([0-9]* ?[0-9]+\\.[0-9]+)".r
  val SUM_PATTERN: Regex = "[0-9]* ?[0-9]+.[0-9]+".r
  val COMPANY_NAME_PATTERN: Regex = "Филиал ([а-яА-Я]* [а-яА-Яa-zA-Z]* [а-яА-Я])".r
  val COMPANY_BIN_PATTERN: Regex = "БИН ([0-9]{12})".r
  val COMPANY_ZNM_PATTERN: Regex = "ЗНМ: ([A-Z]{3}[0-9]{8,})".r
  val COMPANY_KKM_ID_PATTERN: Regex = "Код ККМ КГД \\(РНМ\\): ([0-9]{12})".r
  val FISCAL_ID_PATTERN: Regex = "Фискальный признак:([0-9]{10})".r
  val COMPANY_ADDRESS_PATTERN: Regex = "(г\\. ?[а-яА-Я-] ?, ?[а-яА-Я-]* ?, ?[а-яА-Я- ]* ?, ?[0-9]* ?, ?[0-9а-яА-Я-])".r
  val DATE_OF_SALE_PATTERN: Regex = "Время: ([0-9]{2}\\.[0-9]{2}\\.[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2})".r
  val TOTAL_SUM_PATTERN: Regex = "ИТОГО:([0-9] ?[0-9]+.[0-9]+)".r
}


object ParserApp extends App{

    val INPUT_FILE = "src/main/raw.txt"
    val OUTPUT_FILE = "src/main/parsedReceipt.json"
    val FILE_CONTENTS = Source.fromFile(INPUT_FILE).getLines.toList

    def helper (list: List[String], items: List[Product], rest: List[String]): (List[Product], List[String]) =
      list match {
        case head :: tail => if (NUMBER_PATTERN.matches(head)) {
          val productList = tail.take(5)
          val productName = productList.take(1).mkString("")
          val (quantity, price) = MULTIPLICATION_PATTERN.findFirstMatchIn(productList.slice(1, 2).toString.replace(',', '.'))

          match {
            case Some(x) => (x.group(1).replaceAll(" ", "").toDouble.toInt, x.group(2).replaceAll(" ", "").toDouble)
            case None => (0, 0.0)
          }
          val sum = SUM_PATTERN.findFirstIn(productList.drop(2).toString.replace(',', '.'))

          match {
            case Some(x) => x.replaceAll(" ", "").toDouble
            case None => 0.0
          }
          helper(tail.drop(5), items :+ Product(productName, quantity, price, sum), rest)
        }
        else {
          helper(tail, items, rest :+ head)
        }

        case Nil => (items, rest)
      }

    def handleMatch(m: Option[Match]): String =
      m match {
        case Some(x) => x.group(1)
        case None => " "
      }

    def createReceipt(items: List[Product], rest: List[String]): Receipt = {
      val str = rest.foldLeft("")((memo, next) => memo + (if (next.last != ':') next + "\n" else next))
      val orgTitle = handleMatch(COMPANY_NAME_PATTERN.findFirstMatchIn(str))
      val orgId = handleMatch(COMPANY_BIN_PATTERN.findFirstMatchIn(str))
      val kkmSerialNumber = handleMatch(COMPANY_KKM_ID_PATTERN.findFirstMatchIn(str))
      val kkmId = handleMatch(COMPANY_KKM_ID_PATTERN.findFirstMatchIn(str))
      val fiscalId = handleMatch(FISCAL_ID_PATTERN.findFirstMatchIn(str))
      val placeAddress = handleMatch(COMPANY_ADDRESS_PATTERN.findFirstMatchIn(str))
      val transactionDate = handleMatch(DATE_OF_SALE_PATTERN.findFirstMatchIn(str))
      val totalSum = handleMatch(TOTAL_SUM_PATTERN.findFirstMatchIn(str)).replace(',', '.').replaceAll(" ", "").toDouble

      Receipt(orgTitle, orgId, kkmSerialNumber, kkmId, fiscalId, placeAddress, transactionDate, items, totalSum)
    }

    def writeFile(fileName: String, content: String): Unit = {
      val pw = new PrintWriter(new File(fileName))
      pw.write(content)
      pw.close()
    }

    def readFile(fileName: String): String = {
      Source.fromFile(fileName).getLines().mkString("\n")
    }

    val (items, rest) = helper(FILE_CONTENTS, List[Product](), List[String]())
    val receipt = createReceipt(items, rest)
    val json = receipt.asJson.toString()
    writeFile(OUTPUT_FILE, json)
    val in_json = readFile(OUTPUT_FILE)
    val decoded = decode[Receipt](in_json)
    println(decoded)

}
