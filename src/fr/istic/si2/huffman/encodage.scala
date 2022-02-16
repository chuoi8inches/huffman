package fr.istic.si2.huffman

object Encodage {

  /**
   * @param c un caractère
   * @param h un arbre de Huffman
   * @return l'encodage de c, selon h, s'il existe
   */
  def encodeSymbol(c: Char, h: Huffman): Option[List[Bit]] = {
    (c,h) match {
      case (a, Feuille(_,b)) => if (a == b) Some(Nil) else None
      case (a, Noeud(_, b, c)) =>
        val x = encodeSymbol(a, b)
        val y = encodeSymbol(a, c)
        (x, y) match {
          case (None, Some(l)) => Some(One::l)
          case (Some(l), _) => Some(Zero::l)
          case _ => None
        }
    }
  }

  /**
   * @param l une liste de caractères
   * @param h un arbre de Huffman
   * @return la séquence de bits correspondants à
   *         l'encodage selon h des éléments de l, s'il a réussi.
   *         Les caractères pour lesquels l'encodage est impossible sont oubliés
   */
  def encodeList(l: List[Char], h: Huffman): List[Bit] = {
    l match {
      case Nil => Nil
      case m :: n =>  (encodeSymbol(m,h)) match {
                        case None => encodeList(n,h)
                        case Some(s) => s ++ encodeList(n,h)
      }
    }
  }

  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  def encode(s: String, h: Huffman): List[Bit] = {
    encodeList(s.toList,h)
  }

}