val romanNumerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

//capitalOfCountry("andorra")
//capitalOfCountry("us")

capitalOfCountry get "andorra"
capitalOfCountry get "US"


class Poly(val terms0: Map[Int, Double]) {
   val terms = terms0 withDefaultValue(0.0)
   def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
   def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
   }
   override def toString =
      (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp)  mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2
p1.terms(7)

