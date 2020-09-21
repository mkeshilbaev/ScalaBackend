object Main extends App{


  def problem1() = {
    def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
      var res: Array[Boolean] = Array()
      var max = candies.max
      for (i <- candies){
        if(candies(i) + extraCandies >= max){
          res = res.appended(true)
        }
        else {
          res = res.appended(false)
        }
      }
      res
    }

    def test1() = {
      val candies: Array[Int] = Array[Int](2, 3, 5, 1, 3)
      val extraCandies: Int = 3
      println(kidsWithCandies(candies, extraCandies))
    }

    test1()
  }



  def problem2() = {
    //https://leetcode.com/problems/convert-binary-number-in-a-linked-list-to-integer/

    class ListNode(_x: Int = 0, _next: ListNode = null) {
      var next: ListNode = _next
      var x: Int = _x
    }

    def prepare(array: Array[Int]): ListNode = {
      var node: ListNode = null
      for (x <- array.reverse) {
        val curNode = new ListNode(x)
        curNode.next = node
        node = curNode
      }
      node
    }

    def traverse(head: ListNode, res: String): String = {
      if (head.next == null) return res + " " + head.x
      traverse(head.next, res + " " + head.x)
    }

    def getDecimalValue(head: ListNode): Int = {
      def rec(head: ListNode, res: String): String = {
        if (head.next == null) return res + head.x
        rec(head.next, res + head.x)
      }

      Integer.parseInt(rec(head, ""), 2)
    }

    def test1() = {
      val rawData = Array(1, 0, 1)
      val head: ListNode = prepare(rawData)
      println(getDecimalValue(head))
      //println(traverse(head, ""))
    }

    def test2() = {
      val rawData = Array(1,0,0,1,0,0,1,1,1,0,0,0,0,0,0)
      val head: ListNode = prepare(rawData)
      println(getDecimalValue(head))
      //println(traverse(head, ""))
    }

    test2()
  }



    def problem3() = {
      //https://leetcode.com/problems/how-many-numbers-are-smaller-than-the-current-number/

      def smallerNumbersThanCurrent(nums: Array[Int]): Array[Int] = {
        var res = Array.ofDim[Int](nums.length)
        for ((value, index) <- nums.zipWithIndex){
          var cnt = 0
          nums.foreach(x => if (x < value) cnt += 1)
          res(index) = cnt
        }
        res
      }

      def test1() = {
        val nums: Array[Int] = Array[Int](8,1,2,2,3)
        println(smallerNumbersThanCurrent(nums))
      }
      test1()
    }



  def problem4() = {
    //https://leetcode.com/problems/n-repeated-element-in-size-2n-array/

    def repeatedNTimes(A: Array[Int]): Int = {
      val res = A.map(x => A.count(y => x == y))
      for((value, index) <- res.zipWithIndex)
        if (value == A.length / 2) return  A(index)
      -1
    }

    def test1() = {
      val nums: Array[Int] = Array(1,2,3,3)
      println(repeatedNTimes(nums))
    }
    test1()
  }



  def problem5() = {
    //https://leetcode.com/problems/decompress-run-length-encoded-list/

    def decompressRLElist(nums: Array[Int]): Array[Int] = {
      (for (i <- nums.indices by 2) yield List.fill(nums(i))(nums(i + 1))).flatten.toArray
    }

    def test1() = {
      val nums: Array[Int] = Array(1,2,3,4)  //  (2 4 4 4)
      println(decompressRLElist(nums))
    }
    test1()
  }



  def problem6() = {
    //https://leetcode.com/problems/find-n-unique-integers-sum-up-to-zero/

    def sumZero(n: Int): Array[Int] = {
      val res = new Array[Int](n)
      val mid = n / 2

      for (i <- 0 to n) {
        res(i) = -mid + i
      }
      if (n % 2 == 0)
        res(n/2) = n/2

      res
    }

    def test1() = {
      val n : Int = 5
      println(sumZero(n))
    }
  }


  def problem7() = {
    //https://leetcode.com/problems/the-k-weakest-rows-in-a-matrix/

    def kWeakestRows(mat: Array[Array[Int]], k: Int): Array[Int] = {
      ???
    }

    def test1() = {
      val arr: Array[Array[Int]] = Array((1,1,0,0,0), (1,1,1,1,0), (1,0,0,0,0), (1,1,0,0,0), (1,1,1,1,1))
      val k : Int = 3
      println(kWeakestRows(arr, k))
    }
  }


  def problem9() = {
    //https://leetcode.com/problems/intersection-of-two-arrays/

    def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
      nums1.filter(nums2.contains(_)).distinct
    }

    def test1() = {
      val nums1 = Array(1,2,2,1)
      val nums2 = Array(2,2)
      println(intersection(nums1, nums2))
    }

    test1()
  }



  def problem10() = {
    //https://leetcode.com/problems/build-an-array-with-stack-operations/

    def buildArray(target: Array[Int], n: Int): List[String] = {
      var num = 1
      var i = 0
      var ans: List[String] = List()

      while (num <= n && i < target.length) {
        if(target(i) != num){
          ans = ans.appended("Push")
          ans = ans.appended("Pop")
          num += 1
        }
        else {
          ans = ans.appended("Push")
          num += 1
          i += 1
        }
      }
      ans
    }

    def test1() = {
      val target = Array(1,3)
      val n = 3
      println(buildArray(target, n))
    }
    test1()
}
