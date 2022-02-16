package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

import fr.istic.si2.huffman._

class TestsHuffman {

  /**
   * Un arbre à utiliser dans tous les tests
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

   val l: List[(Char, Double)] = ('a',0.25)::('b',0.21)::('c',0.18)::('d',0.14)::('e',0.09)::('f',0.07)::('g',0.06)::Nil
   val lhuff: List[Huffman] = Feuille(0.05,'a')::Feuille(0.41,'s')::Feuille(0.1,'b')::Feuille(0.4,'c')::Noeud(0.04,Feuille(0.02,'m'),Feuille(0.02,'n'))::Nil
  /**
   * Test d'encodage d'un caractère 
   */
  @Test
  def testEncodeSymbol() {
    
     assertEquals(encodeSymbol('c',h),Some(Zero :: One :: Zero :: Nil))
     assertEquals(encodeSymbol('z',h),None)
  }
   /**
   * Test d'encodage d'une liste de caractères 
   */
  @Test
  def testEncodeList(){
    assertEquals(encodeList('a' :: 'b' :: 'c' :: Nil,h),Zero :: Zero :: One :: Zero :: Zero :: One :: Zero :: Nil)
  }
   /**
   * Test d'encodage d'une chaine de caractères 
   */
  @Test
  def testEncode() {
    assertEquals(encode("aaa",h),Zero :: Zero :: Zero :: Zero :: Zero :: Zero :: Nil) 
  }
   /**
   * Test de decodage d'une chaine de bits en un caractère 
   */
  @Test
  def testDecodeSymbolv0() {
    assertEquals(decodeSymbolv0(h, Zero :: Zero :: Nil),Some('a'))
  }
   /**
   * Test de fonction auxiliaire: decodage d'une chaine de bits en premier caractere decodable et retourner le reste a decoder
   */
  @Test
  def testDecodeSymbol(){
    assertEquals((decodeSymbol(h,Zero :: Zero :: One :: Nil)),(Some('a'),One :: Nil))
    assertEquals(decodeSymbol(h, Nil),(None,Nil))
    assertEquals(decodeSymbol(h,Zero :: Zero :: One :: One :: One :: Nil),(Some('a'),One :: One :: One :: Nil))
    assertEquals(decodeSymbol(h,One :: Zero :: One :: One :: Zero :: Nil),(Some('b'),One :: One :: Zero :: Nil))
    assertEquals(decodeSymbol(h,One :: One :: One :: Zero :: Nil),(Some('e'),Zero :: Nil))

  }
   /**
   * Test decodage d'une chaine de bits en une string 
   */
  @Test
  def testDecode(){
    assertEquals(decode(Nil,h),"")
    assertEquals(decode(Zero :: Zero :: One :: Zero :: One :: One :: One :: Nil,h),"abe")
  }
   /**
   * Test to String
   */
  @Test
  def testtoString() {
    assertEquals(HuffmanApp0.toString(Zero :: Zero :: One :: Zero :: One :: One :: One :: Nil),"0010111")
  }
  /**
   * Test initHuffman
   */
  
 @Test 
 def testInitHuffman(){
    assertEquals(ConstructionCode.initHuffman(l),Feuille(0.25,'a')::Feuille(0.21,'b')::
        Feuille(0.18,'c')::Feuille(0.14,'d')::Feuille(0.09,'e')::Feuille(0.07,'f')::Feuille(0.06,'g')::Nil)
 }
 /**
  * Test triSelonFreq
  */
 @Test
 def testTriSelonFreq(){
   assertEquals(ConstructionCode.triSelonFreq(lhuff),Noeud(0.04,Feuille(0.02,'m'),Feuille(0.02,'n'))::
       Feuille(0.05,'a')::Feuille(0.1,'b')::Feuille(0.4,'c')::Feuille(0.41,'s')::Nil)
 }
 /**
  * Test insertion
  */
 @Test
 def testInsertion(){
   assertEquals(ConstructionCode.insertion(Feuille(0.25,'x'),Noeud(0.04,Feuille(0.02,'m'),Feuille(0.02,'n'))::
       Feuille(0.05,'a')::Feuille(0.1,'b')::Feuille(0.4,'c')::Feuille(0.41,'s')::Nil),Noeud(0.04,Feuille(0.02,'m'),Feuille(0.02,'n'))::
       Feuille(0.05,'a')::Feuille(0.1,'b')::Feuille(0.25,'x')::Feuille(0.4,'c')::Feuille(0.41,'s')::Nil)
 }
 /**
  * Test uneFusion
  */
 @Test
 def testUneFusion(){
   assertEquals(ConstructionCode.uneFusion(lhuff),Noeud(0.09,Noeud(0.04,Feuille(0.02,'m'),Feuille(0.02,'n')),Feuille(0.05,'a'))::
       Feuille(0.1,'b')::Feuille(0.4,'c')::Feuille(0.41,'s')::Nil)
 }
 /**
  * Test fusion
  */
 @Test 
 def testFusion(){
   assertEquals(ConstructionCode.fusion(lhuff),Noeud(1.0,Feuille(0.41,'s'),Noeud(0.5900000000000001,
       Noeud(0.19,Noeud(0.09,Noeud(0.04,Feuille(0.02,'m'),Feuille(0.02,'n')),Feuille(0.05,'a')),Feuille(0.1,'b')),Feuille(0.4,'c'))))
   // probleme du type Double en Scala : 0.19 + 0.4 = 0.5900000000000001 ! (et pas 0.59) 
 }
 /**
  * Test du codeHuffman
  */
 @Test
 def testcodeHuffman(){
   assertEquals(ConstructionCode.codeHuffman(l),Noeud(1.0,Noeud(0.43,Feuille(0.21,'b'),Noeud(0.22,Feuille(0.09,'e'),
       Noeud(0.13,Feuille(0.06,'g'),Feuille(0.07,'f')))),Noeud(0.5700000000000001,Feuille(0.25,'a'),Noeud(0.32,Feuille(0.14,'d'),Feuille(0.18,'c')))))
 }

}
