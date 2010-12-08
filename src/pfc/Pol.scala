package pfc                                 

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers     

private case class Term(coef: Double, exp: Int) {
	require(coef != 0 && exp >= 0)   
	                 
	private def printCoefWithCorrectFormat = {   
		val coefInt: Int = coef.toInt
		val coefDouble: Double = coefInt
	
		if(coef == coefDouble) 
			coefInt.abs.toString
		else
			coef.abs.toString
	 }
	
	private def printCoef = 
		if(coef.abs != 1 || exp == 0) 
			printCoefWithCorrectFormat 
		else ""
	
	private def printX = if(exp != 0) "x" else ""
	                          
	private def printExp = if(exp != 1 && exp != 0) "^" + exp.toString else ""
	
	override def toString = printCoef + printX + printExp 
	
	def + (that: Term) = {                  
		require(this.exp == that.exp)
		Term(this.coef + that.coef, this.exp)
	}     
	
	def / (that: Term) = Term(this.coef / that.coef, this.exp - that.exp)
}

class Pol private (private val terms: List[Term]) {

	private def this(term: Term) = this(List(term))                       
	private def this(coef: Double, exp: Int) = this(Term(coef, exp))

  	def + (that: Pol): Pol = (this.terms, that.terms) match {
		case (Nil, Nil) => Pol(0)
		case (Nil, h::t) => that
		case (h::t, Nil) => this        
		case (Term(c1,e1)::t1, Term(c2,e2)::t2) => {
			if (e1 == e2) {      
				val sum = c1 + c2
 				val rest = new Pol(t1) + new Pol(t2)
				if(sum == 0)
					new Pol(rest.terms)
				else
					new Pol(Term(c1 + c2, e1)::rest.terms)   			
			}
			else if(e1 > e2){                 
				val rest = new Pol(t1) + that
				new Pol(Term(c1,e1)::rest.terms)
			}
			else {
				val rest = this + new Pol(t2)
				new Pol(Term(c2,e2)::rest.terms)
			}
		}
	}        
	
	
	def - (that: Pol): Pol = this + (-that)   
	
  	def * (that: Pol): Pol = {       
		val termsMultiplied = for(t1 <- this.terms;	t2 <- that.terms)
			yield new Pol(Term(t1.coef * t2.coef, t1.exp + t2.exp))                                                        
		termsMultiplied.foldRight(Pol(0))(_+_)
	}
	
    def / (that: Pol): Tuple2[Pol, Pol] = {                  
		require(that != Pol(0))
		if (this == Pol(0) || this.degree < that.degree)
			(Pol(0), this)
		else {
			val newTerm = new Pol(this.terms.head / that.terms.head)
			var rest = this - (newTerm * that)
			val tuple = rest / that
			val result = tuple._1 + newTerm
			rest = tuple._2
			(result, rest)
		}
	} 

  	def unary_+ : Pol = this

 	def unary_- : Pol = this * -1

  	def * (d: Double): Pol = {    
		new Pol(this.terms.map(term => Term(term.coef * d, term.exp)))
	}
	
 	def / (d: Double): Pol = (this / Pol(d))._1 

  	def degree: Int = if(terms.isEmpty) 0 else terms.head.exp

	def ^(n: Int): Pol = {
		var result = Pol(1,0)
	    for(i <- 1 to n) result *= this
	    result
	}

  	def deriv: Pol = {                 
	    val result = for (term <- terms if (term.exp != 0)) yield { 
			Term(term.coef * term.exp, term.exp - 1)
		}                                             
		new Pol(result)
	}
	
 	def ! : Pol = deriv    

 	def apply(x: Double): Double = terms.foldRight(0.0){
		(term, acc) => acc + (term.coef * (Math.pow(x,term.exp)))
	}
	
  	def apply(that: Pol): Pol = terms.foldRight(Pol(0)){
		(term, acc) => acc + ((that ^ term.exp) * term.coef)
	}

  	override def equals(other: Any): Boolean = other match{
		case that:Pol => {this.terms == that.terms} 
		case _ => false
	}
	
  	override def hashCode: Int = terms.foldRight(0) {(term, acc) => acc + term.hashCode}

	def toStringR :String = {
		if(terms.isEmpty)
			""
		else {	                  
			if(terms.head.coef < 0)
				" - " + terms.head.toString + new Pol(terms.tail).toStringR
			else
				" + " + terms.head.toString + new Pol(terms.tail).toStringR
		}		
	}   
	
 	override def toString = {
		if(terms.isEmpty)
			"0"
		else if(terms.head.coef < 0)
			"-" + terms.head.toString + new Pol(terms.tail).toStringR
		else
			terms.head.toString + new Pol(terms.tail).toStringR
	}
}

object Pol {          
	private val polZero = new Pol(Nil)

	def apply(coef: Double, exp:Int) = new Pol(coef, exp)
  	def apply(coef: Double): Pol = if (coef == 0) polZero else Pol(coef, 0)
	
  	implicit def doubleToPol(d: Double): Pol = Pol(d)
}
   


class PolSpec extends Spec with ShouldMatchers {
	
	describe ("A Term") {
 		it ("should have a coef and an exp") {
			val term = Term(2,3)
			term.coef should equal (2)
			term.exp should equal (3)
		}    
	
		it ("should not have a coef equal to zero") {
			evaluating {Term(0,3)} should produce [IllegalArgumentException]
		}                                        
	
		it ("should not have a exp lesser than zero") {
			evaluating {Term(2,-3)} should produce [IllegalArgumentException]
		}
	                                  
		it ("should print itself hiding the coef and exp when (1,1)") {
			val term = Term(1,1)
			term.toString should equal ("x")
		}                      
	
		it ("should print itself hiding the coef when (1,2)") {
			val term = Term(1,2)
			term.toString should equal ("x^2")
		}        
	
		it ("should print itself hiding the exp when (2,1)") {
			val term = Term(2,1)
			term.toString should equal ("2x")
		}
	     
		it ("should print itself when (2.357,2)") {
			val term = Term(2.357,2)
			term.toString should equal ("2.357x^2")
		}     
	
		it ("should print itself when (1,0)") {
			val term = Term(1,0)
			term.toString should equal ("1") 	  
		}                                    
	
		it ("should print its own module when (-2,3)") {
			val term = Term(-2,3)
			term.toString should equal ("2x^3") 	  
		}                               
	
		it ("should know how to sum to another") {
			val termA = Term(2,3)
			val termB = Term(2,3)		
			(termA + termB) should equal (Term(4,3)) 	  
		}
	}	                                  
	
	describe ("A Pol") {
		it ("should be instantiated with its companion obejct") {
			Pol(2,3)
		}         
	
		describe ("(when printing)"){
			it ("should print its only term") {
				val pol = Pol(2,3)
				pol.toString should equal ("2x^3")
			}                                     
			
		    it ("should know how to print Nil Pol") {
				val polA = Pol(2,2)
				val polB = Pol(-2,2)
				(polA + polB).toString should equal ("0")		
			}
		}
	
		describe ("(when comparing)"){
			it ("should know how to compare to another") {
				val polA = Pol(2,3)
				val polB = Pol(2,3)
				(polA == polB) should equal (true)
			} 
	    }
	
		describe ("(when summing two Pols)"){
			it ("should know how to sum two terms") {
				val polA = Pol(2,3)
				val polB = Pol(2,3)
				(polA + polB) should equal (Pol(4,3))
			}
	                            
			it ("should know how to sum two different terms") {
				val polA = Pol(1,3)
				val polB = Pol(2,3)
				(polA + polB) should equal (Pol(3,3))
			}      
	
			it ("should know how to sum two terms with diffent exp") {
				val polA = Pol(1,1)
				val polB = Pol(2,2)
				(polA + polB).toString should equal ("2x^2 + x")
			}             
	
			it ("should know how to sum two Pols iguais with 3 terms each") {
				val polA = Pol(2,2) + Pol(1,1) + Pol(1,0)
				val polB = Pol(2,2) + Pol(1,1) + Pol(1,0)
				(polA + polB).toString should equal ("4x^2 + 2x + 2")
			}                   
	
			it ("should know how to sum two Pols with different number of terms") {
				val polA = Pol(2,2) + Pol(1,1) + Pol(1,0)
				val polB = Pol(2,3) + Pol(1,1)
				(polA + polB).toString should equal ("2x^3 + 2x^2 + 2x + 1")		
			}

			it ("should know how to sum two Pols with negative coefs") {
				val polA = Pol(-2,2) + Pol(1,1) + Pol(-4,0)
				val polB = Pol(3,2) + Pol(-3,1) + Pol(3,0)
				(polA + polB).toString should equal ("x^2 - 2x - 1")		
			}  
			
			it ("should know how to filter null terms") {
				val polA = Pol(2,2) + Pol(1,1)
				val polB = Pol(-2,2)
				(polA + polB).toString should equal ("x")		
			}    
	    } 
	
		describe ("(when creating itself)"){	             
			it ("should know how to create a Pol reverse order of exp") {
				val polA = Pol(-2,3) + Pol(1,1) + Pol(-4,4)
				(polA).toString should equal ("-4x^4 - 2x^3 + x")		
			}
		} 
		
		describe ("(when multiplying a Double)"){
			it ("should know how to multiply an only term Pol by a number") {
				val polA = Pol(1,1)
				(polA * 2).toString should equal ("2x")
			} 
			      
			it ("should know how to multiply a two terms Pol by a number") {
				val polA = Pol(2,2) + Pol(1,1)
				(polA * 2).toString should equal ("4x^2 + 2x")
			}
		}       
		
		describe ("(when using a unary operator)"){
			it ("should know how to negate an only term Pol by a number") {
				val polA = Pol(1,1)
				(-polA).toString should equal ("-x")
			}        
			
			it ("should know how to use plus with an only term Pol by a number") {
				val polA = Pol(1,1)
				(+polA).toString should equal ("x")
			}        
		}
		
		describe ("(when subtracting)"){
			it ("should know how to subtract two Pols") {
				val polA = Pol(2,5) + Pol(1,4) + Pol(1,3)                
				val polB = Pol(1,5) + Pol(2,4) + Pol(1,2) + Pol(-1,1)            				
				(polA - polB).toString should equal ("x^5 - x^4 + x^3 - x^2 + x")
			}               
		}     
		
		describe ("(when asking for degree)"){
			it ("should be 2 for x^2") {
				val polA = Pol(1,2)
				(polA.degree) should equal (2)
			}
			      
			it ("should be 0 for an empty Pol") {
				val polA = Pol(1,1) - Pol(1,1)
				(polA.degree) should equal (0)
			}			         
		}    
		
		describe ("(when applying value into Pol)"){
			it ("should be 4 for x = 2 in x^2") {
				val polA = Pol(1,2)
				(polA(2)) should equal (4)
			}  	         
		} 

		describe ("(when multiplying a Pol)"){
			it ("should know how to multiply two one term Pols") {
				val polA = Pol(2,1)
				val polB = Pol(3,1)
				(polA * polB).toString should equal ("6x^2")
			}
			    
			it ("should know how to multiply an one term Pol by a two term Pol") {
				val polA = Pol(2,1)
				val polB = Pol(3,1) + Pol(2,2)
				(polA * polB).toString should equal ("4x^3 + 6x^2")
			}
			 
	  		it ("should know how to multiply two two terms Pol") {
				val polA = Pol(1,1) + Pol(2,0)
				val polB = Pol(-1,1) + Pol(2,0)
				(polA * polB).toString should equal ("-x^2 + 4")
			}  	
		}    
		 
		describe ("(when solving an exponentiation)"){  
			it ("should be x for (x)^1") {
				val polA = Pol(1,1)
		       	(polA ^ 1).toString should equal ("x")
		    }
			     
		    it ("should be x^2 - 4x + 4 for (x - 2)^2") {
		        val polA = Pol(1,1) - Pol(2,0)
		        (polA ^ 2).toString should equal ("x^2 - 4x + 4")
		    }
		}    
		
		describe ("(when deriving using deriv)"){  
			it ("should be 9x^2 for 3x^3") {
		    	val polA = Pol(3,3)
		       	(polA.deriv).toString should equal ("9x^2")
		    }         
		
		    it ("should be 9x^2 + 8x for 3x^3 + 4x^2") {
		       	val polA = Pol(3,3) + Pol(4,2)
		       	(polA.deriv).toString should equal ("9x^2 + 8x")
		    }
		}	
		
		describe ("(when deriving using ! )"){
			it ("should be 9x^2 for 3x^3") {
		    	val polA = Pol(3,3)
		       	(polA!).toString should equal ("9x^2")
		    }         
		
		    it ("should be 9x^2 + 8x for 3x^3 + 4x^2") {
		       	val polA = Pol(3,3) + Pol(4,2)
		       	(polA!).toString should equal ("9x^2 + 8x")
		    }   
		     
			it ("should be 1 for x + 1") {
		       	val polA = Pol(1,1) + Pol(1,0)
		       	(polA!).toString should equal ("1")
		    }
			
		}
		 
		describe ("(when dividing)"){  
		    it ("should be [-2x, 0] for 2x^2 / x") {
		    	val polA = Pol(2,2)
				val polB = Pol(1,1)     
			    val tuple = polA / polB 
			    val result = tuple._1
			    val rest = tuple._2
			    result.toString should equal ("2x")
			    rest.toString should equal ("0")
		    }      
		}  
		
		describe ("(when operating a Pol with a Number)"){
			it ("should know how to sum") {
				val polA = Pol(1,1) + Pol(1,0)
				(polA + 1).toString should equal ("x + 2")
			} 
			
			it ("should know how to subtract") {
				val polA = Pol(1,1) + Pol(1,0)
				(polA - 1).toString should equal ("x")
			}                                 
			
			it ("should know how to divide") {
				val polA = Pol(2,1) + Pol(2,0)
				(polA / 2).toString should equal ("x + 1")
			}                                 
		}  
		
		describe ("(when composing)"){
			it ("should return (x^2 + 5x + 6) for f(y) = y^2 + y and y = x + 2") {
				val polA = Pol(1,2) + Pol(1,1)
				val polB = Pol(1,1) + Pol(2,0)
				(polA(polB)).toString should equal ("x^2 + 5x + 6")
			} 
		}         	
	}
}
               

