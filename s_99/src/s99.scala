
/**
 * common functions
 * 
 * 
 */
object s99 extends App {
  
  
  println(P1.last(List(1,1,2,3,5,8)))
  println(P2.penultimate(List(1,1,2,3,4,5,6,7,8)))
  println(P3.nth(4, List(1, 1, 2, 3, 5, 8)))
  //println(P3.nth(20, List(1)))
  println(P4.length(List(1,2,3,4,3,4,5)));
  println(P5.reverse(List(1, 1, 2, 3, 5, 8)));
  
  println(P8.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
  
  println(P10.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
  println(P11.encodedModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
  println(P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))));
  println(P13.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)));
  println(P14.duplicate(List('a, 'b, 'c, 'c, 'd)));
  println(P15.duplicateN(3, List('a, 'b, 'c, 'c, 'd)));
  println(P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
  println(P17.split(5, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
  println(P18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
  println(P19.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
  println(P19.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));
  println(P20.removeAt(1, List('a, 'b, 'c, 'd)))
  println(P21.insertAt('new, 1, List('a, 'b, 'c, 'd)))
  println(P22.range(4, 9))
  println(P23.randomSelect(8, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
  println(P24.lotto(6, 49))
  println(P25.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
  println(P26.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
}