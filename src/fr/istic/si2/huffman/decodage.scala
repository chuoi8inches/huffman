package fr.istic.si2.huffman

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return caractère correspondant au décodage de l selon h
   */
  // TODO V0
  def decodeSymbolv0(h: Huffman, l: List[Bit]): Option[Char] = {
    (h,l) match {
      case (Feuille(_, a), Nil) => Some(a)
      case (Feuille(_, a), _::_) => None
      case (_, Nil) => None
      case (Noeud(_, a, _), Zero::m) => decodeSymbolv0(a,m)
      case (Noeud(_, _, b), One::m) => decodeSymbolv0(b,m)
    }
  }
 

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage de l selon h
   *         - deuxième composante : la liste des bits restant à décoder après avoir suivi l dans h.
   */
  def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
     l match {
      case Nil => (None, Nil)
      case _ => 
        decodeSymbolv0(h, l) match {
          case Some(c) => (Some(c), Nil)
          case None => 
            decodeSymbol(h,l.dropRight(1)) match {
              case (x, y) => (x, y :+ l.last)
            }
        }
        
      }
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h
   */
  def decode(l: List[Bit], h: Huffman): String = {
    decodeSymbol(h,l) match {
      case (None, _) => ""
      case (Some(l),r) => l + decode(r,h)  
    }
  }

}