package pfc

private class Pol (private val terms: List[Term]) {

  // construtor auxiliar
  // (n.b.: tanto o construtor primario como o auxiliar sao privados)
//  private def this(coef: Double, exp: Int)
//
//  // aritmetica de polinomios
//  def + (that: Pol): Pol
//  def - (that: Pol): Pol
//  def * (that: Pol): Pol
//  def / (that: Pol): Tuple2[Pol, Pol]
//
//  // operadores unarios
//  def unary_+ : Pol
//  def unary_- : Pol
//
//  // aritmetica mista (o operando 1 e' um polinomio, o operando 2 e' um numero)
//  def + (d: Double): Pol
//  def - (d: Double): Pol
//  def * (d: Double): Pol
//  def / (d: Double): Pol
//
//  // grau, potenciacao e derivacao
//  def degree: Int
//  def ^(n: Int): Pol
//  def deriv: Pol
//  def ! : Pol
//
//  // calcula o valor do polinomio alvo para um dado valor de x
//  def apply(x: Double): Double
//
//  // composicao do polinomio alvo com outro polinomio
//  def apply(that: Pol): Pol
//
//  // sobrescrita de metodos da classe Any
//  override def equals(other: Any): Boolean
//  override def hashCode: Int
//  override def toString
//
//  // metodo auxiliar que multiplica o polinomio alvo por um termo simples
//  private def * (term: Term): Pol
}

object Pol {

  // conversao implicita de Double em Pol
//  implicit def doubleToPol(d: Double): Pol
//
//  // metodos de fabrica acessiveis para os clientes
//  def apply(coef: Double, exp: Int): Pol
//  def apply(coef: Double): Pol
//
//  // metodo de fabrica interno (serve apenas para evitar o uso de new)
//  private def apply(terms: List[Term]): Pol
//
//  // metodo auxiliar para as operacoes de adicao e subtracao de polinomios
//  private def add(terms1: List[Term], terms2: List[Term]): List[Term]
}
