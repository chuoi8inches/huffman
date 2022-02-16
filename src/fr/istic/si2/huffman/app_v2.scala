package fr.istic.si2.huffman

import scala.io.Source
import java.io.{File, PrintWriter}
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._

import scala.io.StdIn.readLine

/**
 * Application principale
 */
object HuffmanApp extends App {

  /**
   * Une liste de couples caractère / fréquence d'apparition
   * à utilisée par l'application principale.
   */
  val lfreqs: List[(Char, Double)] = ('a',0.25)::('b',0.21)::('c',0.18)::('d',0.14)::('e',0.09)::('f',0.07)::('g',0.06)::Nil
  val listhuff: List[Huffman] = Feuille(0.05,'a')::Feuille(0.41,'s')::Feuille(0.1,'b')::Feuille(0.4,'c')::Noeud(0.04,Feuille(0.02,'m'),Feuille(0.02,'n'))::Nil
  /**
   * @param nom le nom d'un fichier
   * @return la chaîne contenue dans le fichier nommé nom
   */
  def lireFichier(nom: String): String = {
    Source.fromFile(nom).getLines.mkString
  }

  /**
   * Ecrit une chaîne de caractères dans un fichier.
   *  Le fichier est écrasé s'il était déjà existant.
   *
   * @param nom le nom du fichier dans lequel on écrit
   * @param contenu la chaîne de caractères à écrire
   */
  def ecrireFichier(nom: String, contenu: String): Unit = {
    val writer = new PrintWriter(new File(nom))
    writer.write(contenu)
    writer.close()
  }

  // V2 l'application principale
 
  val h1 = codeHuffman(lfreqs)
  println("Saisir le nom du fichier:")
  val filename = readLine()
  val f = lireFichier(filename)
  val h2 = codeHuffman(analyseFrequences(f)) // V2 utiliser l'analyse des frequences
  val v1 = HuffmanApp0.toString(Encodage.encode(f,h1))
  val v2 = HuffmanApp0.toString(Encodage.encode(f,h2))
  ecrireFichier("fichiers/out1.txt",v1)
  ecrireFichier("fichiers/out2.txt",v2)
  
  //////////////////////////////////////////////////////////////////////
  val v = nettoyerTexte("Bonjour je m'appelle Delphine".toList)
  println(v)
  println(enleverDoublon(v))
  println(compteCaractere('l',v))
  println(analyseFrequences("Bonjour je m'appelle Delphine"))
  
}