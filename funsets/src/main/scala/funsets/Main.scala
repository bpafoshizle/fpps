package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)

  val s1neg = singletonSet(-1)
  val s2neg = singletonSet(-2)
  val s3neg = singletonSet(-3)

  val soob = singletonSet(5)

  val s12 = union(s1,s2)
  val s123 = union(union(s1,s2),s3)

  val s123neg = union(union(s1neg,s2neg),s3neg)
  val s321neg = union(union(s3neg,s2neg),s1neg)

  printSet(s123)
  printSet(filter(s123, (x: Int) => x != 1))
  printSet(filter(s123, (x: Int) => false))

  println(exists(s123, (x: Int) => x < 0))

  printSet(map(s123, (x: Int) => x * x))
  printSet(map(s123, (x: Int) => -x))
}
