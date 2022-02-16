package fr.istic.si2.huffman

object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
 
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = {
    l match {
      case Nil => Nil
      case (a,b)::x => Feuille(b,a)::initHuffman(x) 
    }
  }

  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  
  def triSelonFreq(l: List[Huffman]): List[Huffman] = {
    l match {
      case Nil => Nil
      case x::y => insertion(x,triSelonFreq(y))
    }
  }
  /**
   * @param i une Huffman
   * @param l une liste de Huffmans trie selon frequence
   * @return une liste de Huffmans trie avec i a la bon place 
   */
   def insertion(i: Huffman,l: List[Huffman]): List[Huffman] = {
    (i,l) match {
      case (a,Nil) => a :: Nil
      case (Feuille(x,y),Feuille(z,t)::m) => if (x<=z) {Feuille(x,y)::Feuille(z,t)::m}
                                             else Feuille(z,t)::insertion(Feuille(x,y),m)
      case (Feuille(x,y),Noeud(z,t,v)::m) => if (x<=z) {Feuille(x,y)::Noeud(z,t,v)::m}
                                             else Noeud(z,t,v)::insertion(Feuille(x,y),m)
      case (Noeud(z,t,v),Feuille(x,y)::m) => if (z<=x) {Noeud(z,t,v)::Feuille(x,y)::m}
                                             else Feuille(x,y)::insertion(Noeud(z,t,v),m)
      case (Noeud(z,t,v),Noeud(a,b,c)::m) => if (z<=a) {Noeud(z,t,v)::Noeud(a,b,c)::m}
                                             else Noeud(a,b,c)::insertion(Noeud(z,t,v),m)                                      
  }
}
  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = {
    fusion(initHuffman(freqs))
  }

  /**
   * @param l une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
 
  def uneFusion(l: List[Huffman]): List[Huffman] = {
  val z = triSelonFreq(l)
    z match {
      
      case Feuille(a,b)::Feuille(c,d)::m =>  Noeud(a + c,Feuille(a,b),Feuille(c,d))::m
                                            
      case Feuille(a,b)::Noeud(c,d,e)::m =>  Noeud(a + c,Feuille(a,b),Noeud(c,d,e))::m
                                           
      case Noeud(a,b,c)::Feuille(d,e)::m =>  Noeud(a + d,Noeud(a,b,c),Feuille(d,e))::m
                                            
      case Noeud(a,b,c)::Noeud(d,e,f)::m =>  Noeud(a + d,Noeud(a,b,c),Noeud(d,e,f))::m
                                            
      case _ => l
    }
  }

  /**
   * @param l une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  
  def fusion(l: List[Huffman]): Huffman = {
    l match {
      case n::Nil => n
      case x::y => fusion(uneFusion(x::y))
      case _ => Feuille(0, '_')
    }
  }

  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
 
  def analyseFrequences(s: String): List[(Char, Double)] = {
    val s1 = s.toLowerCase
    val characters = nettoyerTexte(s1.toList)
    val characters2 = enleverDoublon(characters)
    calculeFrequenceList(characters, characters2)
  }
    
  /**
   * @param l une liste de caractere 
   * @return la liste l qui n'a pas des caractere special
   */
  
  def nettoyerTexte(l: List[Char]): List[Char] = {
    l match {
      case Nil => Nil
      case ' '::x => nettoyerTexte(x)
      case '.'::x => nettoyerTexte(x)
      case ','::x => nettoyerTexte(x)
      case '\''::x => nettoyerTexte(x)
      case a::x => a::nettoyerTexte(x)
    }
  }
  /**
   * @param c une caractere 
   * @param l une liste de caractere 
   * @return la nombre d'apparition de c dans l
   */
  def compteCaractere(c: Char, l:List[Char]): Int = {
    l match {
      case Nil => 0
      case b :: x => if  ( b==c ) 1 + compteCaractere(c,x) 
                     else compteCaractere(c,x)
      
    }
  }
  /**
   * @param l une liste de caractere
   * @return une liste de caractere qui n'a pas les caractere repete de l
   */
  def enleverDoublon(l: List[Char]): List[Char] = {
    l match {
      case Nil => Nil
      case b :: x => if (x.contains(b)) enleverDoublon(x)
                     else b :: enleverDoublon(x)
    }
  }
  /**
   * @param l1 une liste de caractere qui n'a pas de caractere special
   * @param l2 une liste de caractere qui n'a pas de caractere repete
   * @return une liste de couples a caractere/frequence selon l1, l2
   */
  def calculeFrequenceList(l1: List[Char], l2: List[Char]): List[(Char, Double)] = {
    val length = l1.length
    l2 match {
      case Nil => Nil
      case a::b => (a, compteCaractere(a, l1) / length.toDouble)::calculeFrequenceList(l1, b)
    }
  }
}
