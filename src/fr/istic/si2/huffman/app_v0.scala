package fr.istic.si2.huffman

import scala.io.Source
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

/**
 * Type algébrique simple modélisant les bits (0 ou 1)
 */
sealed trait Bit
case object Zero extends Bit
case object One extends Bit

/**
 * Type algébrique récursif modélisant les arbres de code de Huffman
 */
sealed trait Huffman
case class Feuille(freq: Double, c: Char) extends Huffman
case class Noeud(freq: Double, zero: Huffman, one: Huffman) extends Huffman

/**
 * Application principale V0
 */
object HuffmanApp0 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  val h: Huffman = Noeud(1.0,
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
   * @param l une liste de bits
   * @return la chaîne de caractères où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */
  
  def toString(l: List[Bit]): String = {
    l match {
      case Nil => ""
      case Zero :: m => 0 + toString(m)
      case One :: m => 1 + toString(m)
    }
  }

  /**
   * application principale v0
   */
 def mainApp(): Unit = {
     for (a <- 'a' to 'z') {
       val encode = encodeSymbol(a, h)
       val encodeToString = 
          encode match {
               case None => "_"
               case Some(l) => toString(l)
             }
       val decode = 
           encode match {
             case None => None
             case Some(l) => decodeSymbolv0(h, l)
           }
       val decodeToString = 
         decode match {
                 case None => "_"
                 case Some(c) => c
               }
       println(a + "  " + encode + "  " + encodeToString + "  " + decodeToString)
       
     }
     
 }
  mainApp()



}