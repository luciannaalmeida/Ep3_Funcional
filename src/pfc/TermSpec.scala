package pfc


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers 
import pfc._

class TermSpec extends FlatSpec with ShouldMatchers {
	"A Term" should "have a coef and an exp" in {
		val term = Term(2,3)
		term.coef should equal (2)
		term.exp should equal (3)
	}
	
}

//
//class StackSpec extends FlatSpec with ShouldMatchers {
//
//  "A Stack" should "pop values in last-in-first-out order" in {
//    val stack = new Stack[Int]
//    stack.push(1)
//    stack.push(2)
//    stack.pop() should equal (2)
//    stack.pop() should equal (1)
//  }
//
//  it should "throw NoSuchElementException if an empty stack is popped" in {
//    val emptyStack = new Stack[String]
//    evaluating { emptyStack.pop() } should produce [NoSuchElementException]
//  }
//}
