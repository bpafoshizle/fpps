import patmat.Huffman._

val sampleTree = makeCodeTree(
  makeCodeTree(Leaf('x',1),Leaf('e',1)),
  Leaf('t',2)
)

times(List('a', 'b', 'a'))

val tl = List(
  ("a",1),
  ("b",1),
  ("c",1),
  ("a",1),
  ("b",1),
  ("a",1)
)


times(string2Chars("abcdaba"))

def numberList(l: List[Char]): List[(Char, Int)] = {
  if(l.isEmpty) Nil
  else List((l.head, 1)) ::: numberList(l.tail)
}

val nl = numberList(string2Chars("abcdaba"))
val nlg = nl.groupBy(_._1)
val mvnlg = nlg.mapValues(_.size).toList

val tlGrouped = tl.groupBy(_._1) //tlGrouped: scala.collection.immutable.Map[String,List[(String, Int)]] = Map(b -> List((b,1)), a -> List((a,1), (a,1)), c -> List((c,1)))
val tlMapValues = tlGrouped.mapValues(_.size).toList
//val result = tl.groupBy(_._1).mapValues(_.map(_._2).sum).toList


makeOrderedLeafList(times(string2Chars("abcdaba")))


val ctl = List(sampleTree)
singleton(ctl)
singleton(List())
singleton(ctl ::: List(makeCodeTree(Leaf('a',3), Leaf('b',2))))


val combT = combine(makeOrderedLeafList(times(string2Chars("abcdabavv"))))

val st1 = makeCodeTree(sampleTree, makeCodeTree(Leaf('a',3), Leaf('b',2)))

val myCodeTree = createCodeTree(string2Chars("abcdabavv"))

decodedSecret

val t3 = createCodeTree(string2Chars("Premature peformance tuning is the root of all evil"))
encode(t3)(List('P'))
decode(t3,encode(t3)(List('P')))

val tb = convert(t3)
tb.filter(_._1 == 'a')(0)


codeBits(tb)('v')
codeBits(tb)('a')
quickEncode(t3)("av".toList)

