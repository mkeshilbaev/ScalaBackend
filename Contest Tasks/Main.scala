object Main extends App {


      def problem938() = {
    //https://leetcode.com/problems/range-sum-of-bst/
        class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
          var value: Int = _value
          var left: TreeNode = _left
          var right: TreeNode = _right
        }

        def rangeSumBST(root: TreeNode, L: Int, R: Int): Int = {
          var sum = 0

          if (root == null)
            sum

          var queue = scala.collection.mutable.Queue[TreeNode]()
          queue.enqueue(root)

          while(!queue.isEmpty){
            var current : TreeNode = queue.dequeue()
            if (current.value >= L && current.value <= R)
              sum += current.value

            if (current.left != null && current.value > L)
              queue.enqueue(current.left)

            if (current.right != null && current.value < R)
              queue.enqueue(current.right)
          }
          sum
        }
      }



    def problem1137() = {
      //https://leetcode.com/problems/n-th-tribonacci-number/

      def tribonacci(n: Int): Int = {
        var cur = 3
        var nums = Array[Int](0, 1, 1)

        while (n > cur - 1) {
          nums :+= (nums(cur - 1) + nums(cur - 2) + nums(cur - 3))
          cur += 1
        }
        nums(n)
      }
    }



      def problem783() = {
        //https://leetcode.com/problems/minimum-distance-between-bst-nodes/

    // Definition for a binary tree node.
        class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
          var value: Int = _value
          var left: TreeNode = _left
          var right: TreeNode = _right
        }

        var minDistance = Integer.MAX_VALUE
        var prev: TreeNode = new TreeNode()

        def minDiffInBST(root: TreeNode): Int = {
          minDistance = Integer.MAX_VALUE
          traverse(root)
          minDistance
        }

        def traverse(node: TreeNode): Unit = {
          if(node == null)
            return

          traverse(node.left)

          if(prev != null) {
            minDistance = Math.min(minDistance, (node.value - prev.value).abs)
          }
          prev = node
          traverse(node.right)
        }
  }



  def problem687() = {
    //    https://leetcode.com/problems/longest-univalue-path/

    //    Definition for a binary tree node.
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }

    def longestUnivaluePath(root: TreeNode): Int = {
      var maxUnivaluePath = 0

      // Recursively get longest univalue path starting at each node
      def longestUnivaluePathStartAt(root: TreeNode): Int = {
        // Corner case.
        if (root == null) {
          return 0
        }

        // Recursive cases
        var leftLength = longestUnivaluePathStartAt(root.left)
        var rightLength = longestUnivaluePathStartAt(root.right)

        if (root.left != null && root.value == root.left.value) {
          leftLength += 1
        } else {
          leftLength = 0
        }

        if (root.right != null && root.value == root.right.value) {
          rightLength += 1
        } else {
          rightLength = 0
        }

        // Update maxUnivaluePath if needed
        maxUnivaluePath = Math.max(maxUnivaluePath, leftLength + rightLength)

        Math.max(leftLength, rightLength)
      }

      longestUnivaluePathStartAt(root)
      maxUnivaluePath
    }
  }


  def problem560() = {
    //https://leetcode.com/problems/subarray-sum-equals-k/

    def subarraySum(nums: Array[Int], k: Int): Int = {
      var sum: Int = 0
      var res: Int = 0
      var map = new scala.collection.mutable.HashMap[Int, Int]()

      map += (0 -> 1)

      for (i <- 0 until (nums.length)) {
        sum += nums(i)

        if (map.contains(sum - k)) {
          res += map.getOrElse(sum - k, 0)
        }
        map += (sum -> (map.getOrElse(sum, 0) + 1))
      }
      res
    }
  }



}


