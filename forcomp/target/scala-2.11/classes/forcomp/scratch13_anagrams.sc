
/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]


/** `Occurrences` is a `List` of pairs of characters and positive integers saying
  *  how often the character appears.
  *  This list is sorted alphabetically w.r.t. to the character in each pair.
  *  All characters in the occurrence list are lowercase.
  *
  *  Any list of pairs of lowercase characters and their frequency which is not sorted
  *  is **not** an occurrence list.
  *
  *  Note: If the frequency of some character is zero, then that character should not be
  *  in the list.
  */
type Occurrences = List[(Char, Int)]

val w: Word = "tweeabaat"

val i = 2

(for ((k,v) <- w.groupBy((element: Char) => element)) yield (k, v.length)).toList.sorted

val s: Sentence = List("Hello", "Ladies")

(s foldLeft "")(_ ++ _)

val occList = List(('a', 2), ('b', 2), ('c',1))
val occurrences = List(('a', 2), ('b', 2), ('c',1))

for{
   occ_1 <- occurrences
   occ_2 <- List() :: (occurrences dropWhile(p => p._1 <= occ_1._1))
} yield List(occ_1) ++  List(occ_2)

//combinations(aabbba)
val groupedOccList = occList.groupBy(_._1).toList

val complicatedOverProduced = for {
   (letter_1, count_1) <- occList
   i_1 <- 1 to count_1
   (letter_2, count_2) <- occList
   i_2 <- 1 to count_2
   if(letter_1 != letter_2)
} yield List((letter_1,i_1), (letter_2, i_2)).sorted

complicatedOverProduced.distinct

val charSubSets = for{
   occ <- occurrences
   i <- 1 to occ._2
} yield List((occ._1, i))

val pairs = for{
   (letter_1, count_1) <- occurrences
   (letter_2, count_2) <- occurrences dropWhile(p => p._1 <= letter_1)
   i <- 1 to count_1
   j <- 1 to count_2
} yield List((letter_1, i), (letter_2, j))

List() :: (charSubSets ++ pairs)




val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1))
val lad = List(('a', 1), ('d', 1), ('l', 1))

for{
   (letter_1, count_1) <- x
   decr = y filter(p => p._1 == letter_1) match {
      case Nil => 0
      case List((a,b)) => b
   }
   if count_1 - decr != 0
} yield (letter_1, count_1 - decr)

val decr = y filter(p => p._1 == 'r') match {
   case Nil => 0
   case List((a,b)) => b
}

def wordOccurrences(w: Word): Occurrences = {
   (for ((k,v) <- w.toLowerCase().groupBy((element: Char) => element)) yield (k, v.length)).toList.sorted
}

def sentenceOccurrences(s: Sentence): Occurrences = {
   wordOccurrences((s foldLeft "")(_ ++ _))
}

def combinations(occurrences: Occurrences): List[Occurrences] = {
   val charSubSets = for{
      occ <- occurrences
      i <- 1 to occ._2
   } yield List((occ._1, i))

   val pairs = for{
      (letter_1, count_1) <- occurrences
      (letter_2, count_2) <- occurrences dropWhile(p => p._1 <= letter_1)
      i <- 1 to count_1
      j <- 1 to count_2
   } yield List((letter_1, i), (letter_2, j))

   List() :: (charSubSets ++ pairs)
}

combinations(occurrences)

val sentence = List("Linux", "rulez")
val combs = combinations(sentenceOccurrences(sentence))
