import forcomp.Anagrams.{Occurrences, _}

val sentence = List("Linux", "rulez")
def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
   // Accumulator trick was extremely difficult for me to see a path to, had to get some help
   def sentenceAnagramsIter(occs: Occurrences, acc: Sentence): List[Sentence] = occs match {
      case List() => List(acc)
      case occ => {

         // Get the list of only valid combinations of occurrences and the corresponding words
         val validCombs = for {
            comb <- combinations(occs)
            if(dictionaryByOccurrences(comb).nonEmpty)
         } yield (comb, dictionaryByOccurrences(comb))

         // For each valid occurrence combo, build out a sentence with each and the rest of the valid ones
         for {
            comb <- validCombs
            word <- comb._2 // Can have multiple valid words for single combo
            sen <- sentenceAnagramsIter(subtract(occ, comb._1), acc ::: List(word))
         } yield sen
      }
   }

   sentenceAnagramsIter(sentenceOccurrences(sentence), Nil)
}


sentenceAnagrams(sentence).foreach(println)

val occs1: Occurrences = List(('n',1), ('r',1), ('u',1))
val occs2: Occurrences = List(('n',2), ('r',1), ('u',1))
val occs3: Occurrences = List(('n',1), ('r',1))
val occsTest: Occurrences = List(('n',1), ('u',1))
combinations(occs1).exists(x => occsTest.toSet.subsetOf(x.toSet))
combinations(occs2).exists(x => occsTest.toSet.subsetOf(x.toSet))
combinations(occs3).exists(x => occsTest.toSet.subsetOf(x.toSet))








