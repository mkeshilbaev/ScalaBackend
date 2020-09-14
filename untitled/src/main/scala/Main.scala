import scala.io.StdIn.readLine


object Main extends App(){

    println("Enter :")
    val num1 = readLine()
    val op = readLine()
    val num2 = readLine()

    val calc = new Brain()
    calc.calculate(num1, op, num2)
}
