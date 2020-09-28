
case object Main extends App{


//  problem1464()

  def problem1464() = {
//  https://leetcode.com/problems/maximum-product-of-two-elements-in-an-array/
    def maxProduct(nums: Array[Int]): Int = {
      nums.sorted.slice(nums.length - 2, nums.length).reduce((a, b) => (a - 1) * (b - 1))
    }

    def test1() = {
      var nums = Array(3,4,5,2)
      println(maxProduct(nums))
    }
    test1()
  }



//    problem1491()

    def problem1491() = {
//  https://leetcode.com/problems/average-salary-excluding-the-minimum-and-maximum-salary/
      def average(salary: Array[Int]): Double = {
        var res = salary.sorted.slice(1, salary.length - 1).map(x => (x, 1)).reduce((a, b) => (a._1 + b._1, a._2 + b._2))
        var ans = (res._1 / res._2.toDouble)
        println(f"$ans%1.5f")

        ans
      }

      def test1() = {
        var salary = Array(4000,3000,1000,2000)
        average(salary)
      }
      test1()
    }



//  problem1185()

  def problem1185() = {
    //  https://leetcode.com/problems/day-of-the-week/

    def dayOfTheWeek(day: Int, month: Int, year: Int): String = {
      var date: String = ""

      if (day < 10 && month < 10){
        date = "0" + day.toString + "/" + "0" + month.toString + "/" + year.toString
      }
      else if (day < 10 && month >= 10){
        date = "0" + day.toString + "/" + month.toString + "/" + year.toString
      }
      else if (month < 10 && day >= 10){
        date = day.toString + "/" + "0" + month.toString + "/" + year.toString
      }
      else{
        date = day.toString + "/" + month.toString + "/" + year.toString
      }

      val df = java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy")
      val dayOfWeek = java.time.LocalDate.parse(date, df).getDayOfWeek

      dayOfWeek.toString.toLowerCase.capitalize
    }

    def test1() = {
      var day = 2
      var month = 10
      var year = 2019
      println(dayOfTheWeek(day, month, year))
    }
    test1()
  }



//    problem532()

    def problem532() = {
      //  https://leetcode.com/problems/k-diff-pairs-in-an-array/

      def findPairs(nums: Array[Int], k: Int): Int = {
//        if (k < 0)
        //  0
//        else if (k == 0)
//          nums.groupBy(identity).count(_._2.length > 1)
//        else {
//          val s = nums.toSet
//          s.count(n => s.contains(n + k))
//        }

        var sortedNums = nums.sorted
        var hashSet = scala.collection.immutable.HashSet[(Int, Int)]()

        for (i <- 0 until sortedNums.length){
          for (j <- i until sortedNums.length){
            if (i != j && math.abs(sortedNums(i) - sortedNums(j)) == k){
              hashSet += sortedNums(j) -> sortedNums(i)
            }
          }
        }
        hashSet.size
      }

      def test1() = {
        var nums = Array(1, 2, 3, 4, 5)
        var k = 1
        println(findPairs(nums, k))
      }
      test1()
    }

}
