package pfc

private case class Term(coef: Double, exp: Int) {
  require(coef != 0 && exp >= 0)
  
  
}