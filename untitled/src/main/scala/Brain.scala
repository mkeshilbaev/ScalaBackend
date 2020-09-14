import scala.util.{Try, Success, Failure}


class Brain(){


def calculate(num1: String, op: String, num2: String): Unit = {

  def parse(value: String) = Try(value.toDouble)

  (parse(num1), parse(num2)) match {
    case (Success(num1Double), Success(num2Double)) => {
      op match {
        case "/" => println(num1Double / num2Double)
        case "*" => println(num1Double * num2Double)
        case "+" => println(num1Double + num2Double)
        case "-" => println(num1Double - num2Double)
        case invalid: String => println(s"Invalid operator $invalid.")
      }
    }
    case (Failure(e), _) => println(s"Could not parse $num1.")
    case (_, Failure(e)) => println(s"Could not parse $num2.")
    case (Failure(e1), Failure(e2)) => println(s"Could not parse $num1 and $num2.")
  }
 }
}