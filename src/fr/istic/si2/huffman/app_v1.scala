package fr.istic.si2.huffman

import scala.io.StdIn._
import scala.io.Source
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

/**
 * Application principale V1
 */
object HuffmanApp1 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  val h: Huffman =  Noeud(1.0,
                        Noeud(0.57,
                            Feuille(0.25,'a'),
                            Noeud(0.32,
                                Feuille(0.18,'c'),
                                Feuille(0.14,'d'))),                        
                        Noeud(0.43,
                            Feuille(0.21,'b'),
                            Noeud(0.22,
                                Noeud(0.13,
                                  Feuille(0.07,'f'),   
                                  Feuille(0.06,'g')),    
                                Feuille(0.09,'e'))))  

   
  /**
   * @param s une chaîne de caractères
   * @return la chaîne de 0 et 1 représentant chaque caractère
   *         de s par son encodage sur 16 bits
   */
  def vers16Bits(s: String): String = {
    s.toList.map(c => String.format("%16s", c.toBinaryString).replace(' ', '0')).reduce(_ + _)
  }

  /**
   * application principale avec boucle interatif
   */
   def mainAppv1(): Unit = {
           var continue : String = ""
             do {
               println("Chaine a encoder ?")
               val input = readLine().toLowerCase
               println("Chaine encodee standard : ")
               println("    " + vers16Bits(input))
               println("    " + "taille (nb Bits) : " + vers16Bits(input).length)
               println("Chaine encodee Huffman : ")
               val e = encode(input,h) 
               println("    " + HuffmanApp0.toString(e))
               println("    " + "taille (nb Bits) : " + e.length)
               println("Chaine decodee Huffman : ")
               println("    " + decode(e,h))
               println("Encore ? [Y/n]")
               continue = readLine()
             }
            while (continue == "Y" || continue == "y")
    }
    mainAppv1()
}