import akka.actor.typed.{ActorRef, ActorSystem, Props}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}


object Calc {

  trait Operation
  trait BasicOperation extends Operation

  final case class Multiplication(m: Int, n: Int) extends BasicOperation
  final case class Upgrade[Op >: BasicOperation](advancedCalculator: Op => Unit) extends BasicOperation

  class CalculatorServer extends ActorRef[BasicOperation] {
    def typedReceive = {
      case Multiplication(m: Int, n: Int) =>
        println(m + " * " + n + " = " + (m * n))

      case Upgrade(advancedCalculator) =>
        println("Upgrading ...")
        context.become(advancedCalculator)
    }
  }

  object CalculatorUpgrade extends App {

    def main(args: Array[String]): Unit = {
      val system : ActorSystem["CalculatorSystem"]
      val simpleCal: ActorRef[BasicOperation] =
        system.systemActorOf(Props[BasicOperation,
          CalculatorServer], "calculator")

      simpleCal ! Multiplication(5, 1)

      case class Division(m: Int, n: Int) extends Operation

      def advancedCalculator: Operation => Unit = {
        case Multiplication(m: Int, n: Int) =>
          println(m + " * " + n + " = " + (m * n))
        case Division(m: Int, n: Int) =>
          println(m + " / " + n + " = " + (m / n))
        case Upgrade(_) =>
          println("Upgraded.")
      }

      simpleCal ! Upgrade(advancedCalculator)
      val advancedCal = system.actorFor[Operation]
      advancedCal ! Multiplication(5, 3)
      advancedCal ! Division(10, 3)
      advancedCal ! Upgrade(advancedCalculator)
    }
  }
}
