//Jared Huberman
//CSC344 Assignment 3
import scala.util.parsing.combinator._
import scala.io.StdIn.readLine

abstract class MatchTree
//case class S(e:MatchTree) extends MatchTree
case class E(left: MatchTree, right: MatchTree) extends MatchTree
case class E2(p: String, right: MatchTree) extends MatchTree
case class E3(left: MatchTree, right: MatchTree) extends MatchTree
case class T(left: MatchTree, right: MatchTree) extends MatchTree
case class T2(left:MatchTree, right: MatchTree) extends MatchTree
case class F(left: MatchTree, right: MatchTree) extends MatchTree
case class F2(q: String, left: MatchTree) extends MatchTree
case class A(opt1:MatchTree, l: String, opt2: MatchTree) extends MatchTree
case class A2(left: MatchTree, r: String) extends MatchTree
case class C(s: String) extends MatchTree
case class NIL() extends MatchTree

class Parser extends JavaTokenParsers{
  //def s: Parser[MatchTree] = e ^^ {case e => S(e)}
  def e: Parser[MatchTree] = t ~ e2 ^^ {case t ~ e2 => E(t, e2)} | t ^^ {case t => E(t,NIL())}
  def e2: Parser[MatchTree] = "|" ~ e ^^ {case p ~ e3 => E2(p, e3)}
  //def e3: Parser[MatchTree] = t ~ e2 ^^ {case t ~ e2 => E3(t, e2)} | t ^^ {case t => E3(t,NIL())}
  def t: Parser[MatchTree] = f ~ t ^^ {case f ~ t2 => T(f, t2)} | f ^^ {case f => T(f, NIL())}
  //def t2: Parser[MatchTree] = f ~ t2 ^^ {case f ~ t2 => T2(f, t2)} | f ^^ {case f => T2(f, NIL())}
  def f: Parser[MatchTree] = a ~ f2 ^^ {case a ~ f2 => F(a, f2)} | a ^^ {case a => F(a, NIL())}
  def f2: Parser[MatchTree] = "?" ~ f2 ^^ {case q ~ f2 => F2(q, f2)} | "?" ^^ {case q => F2(q, NIL())}
  def a: Parser[MatchTree] = c ^^ {case c => A(c, "", NIL())} | "(" ~ a2 ^^ {case l ~ a2 => A(NIL(), l, a2)} //| c ~ "(" ~ a2 ^^ {case c ~ l ~ a2 => A(c, l, a2)}
  def a2: Parser[MatchTree] = e ~ ")" ^^ {case e ~ r => A2(e, r)}
  def c[C] = ("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" 
      | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "0" | "1" | "2" | "3" | "4"
      | "5" | "6" | "7" | "8" | "9" | ".") ^^ {case ch => C(ch)}
  
  def recurseTree(t: MatchTree){
  //  if(t.isInstanceOf[S]){
    //  println("S")
      //recurseTree(t.asInstanceOf[S].e)
    //}
    /*else*/
//    t match{
//      case E(l, r) =>{
//        println("E")
//        recurseTree(l)
//      }
//    }
    if(t.isInstanceOf[E]){
      println("E")
      recurseTree(t.asInstanceOf[E].left)
      recurseTree(t.asInstanceOf[E].right)
    }
    else if(t.isInstanceOf[E2]){
      println("E2")
      println(t.asInstanceOf[E2].p)
      recurseTree(t.asInstanceOf[E2].right)
    }
    else if(t.isInstanceOf[E3]){
      println("E3")
      recurseTree(t.asInstanceOf[E3].left)
      recurseTree(t.asInstanceOf[E3].right)
    }
    else if(t.isInstanceOf[T]){
      println("T")
      recurseTree(t.asInstanceOf[T].left)
      recurseTree(t.asInstanceOf[T].right)
    }
    else if(t.isInstanceOf[T2]){
      println("T2")
      recurseTree(t.asInstanceOf[T2].left)
      recurseTree(t.asInstanceOf[T2].right)
    }
    else if(t.isInstanceOf[F]){
      println("F")
      recurseTree(t.asInstanceOf[F].left)
      recurseTree(t.asInstanceOf[F].right)
    }
    else if(t.isInstanceOf[F2]){
      println("F2")
      println(t.asInstanceOf[F2].q)
      recurseTree(t.asInstanceOf[F2].left)
    }
    else if(t.isInstanceOf[A]){
      println("A")
      recurseTree(t.asInstanceOf[A].opt1)
      println(t.asInstanceOf[A].l)
      recurseTree(t.asInstanceOf[A].opt2)
    }
    else if(t.isInstanceOf[A2]){
      println("A2")
      recurseTree(t.asInstanceOf[A2].left)
      println(t.asInstanceOf[A2].r)
    }
    else if(t.isInstanceOf[C]){
      println("C")
      println(t.asInstanceOf[C].s)
    }
    else if(t.isInstanceOf[NIL]){
      println("NIL")
    }
  }

 /* def treeCompare (pattern: MatchTree, input: MatchTree, lastT: MatchTree, lastE: MatchTree) : Boolean = {
    (pattern, input) match{
      case(pattern:S, input:S) => println("Both S"); treeCompare(pattern.e, input.e, lastT, lastE);
      case(pattern:E, input:E) => println("Both E"); (pattern.right != NIL() && ((treeCompare(pattern.left, input.left, lastT, input) || treeCompare(pattern.right.asInstanceOf[E2].right.asInstanceOf[E].left, input.left, lastT, input)))) || (pattern.right == NIL() && treeCompare(pattern.left, input.left, lastT, input))
      case(pattern:E2, input:E2) => println("Both E2"); treeCompare(pattern.right, input.right, lastT, lastE);
      case(pattern:T, input:T) => println("Both T"); ((pattern.right != NIL() && input.right != NIL()) && (treeCompare(pattern.left, input.left, input, lastE) && treeCompare(pattern.right, input.right, input, lastE))) || ((pattern.right == NIL() && input.right == NIL()) && treeCompare(pattern.left, input.left, input, lastE)) || ((pattern.right == NIL() && input.right != NIL()) && treeCompare(pattern.left, input.left, input, lastE)) || ((pattern.right != NIL() && input.right == NIL()) && treeCompare(pattern.left, input.left, input, lastE));
      case(pattern:F, input:F) => println("Both F"); treeCompare(pattern.left, input.left, lastT, lastE);
      case(pattern:F2, input:F2) => println("Both F2"); treeCompare(pattern.left, input.left, lastT, lastE);
      case(pattern:A, input:A) => println("Both A"); (pattern.opt2 != NIL() && (treeCompare(pattern.opt2.asInstanceOf[A2].left, lastE, lastT, lastE) && treeCompare(pattern.opt2.asInstanceOf[A2].left.asInstanceOf[E].left, lastT, lastT, lastE))) || (pattern.opt2 == NIL() && treeCompare(pattern.opt1, input.opt1, lastT, lastE))
      case(pattern:C, input:C) => println("Both C"); pattern.s == input.s || pattern.s == ".";
      case(pattern:NIL, input:NIL) => println("Both NIL"); false;
      case default => println(pattern + "," + input); false;
    }
    //return false;
  } */
  
  def treeCompare (pattern: MatchTree, input: MatchTree): Boolean = {
    pattern match{
      case E(left, E2(p, right)) => {
        input match{
          case E(left, NIL()) => {
            treeCompare(pattern.asInstanceOf[E].left, input.asInstanceOf[E].left) | treeCompare(pattern.asInstanceOf[E].right.asInstanceOf[E2].right, input)
          }
          case T(left: Any, right: Any) => treeCompare(pattern.asInstanceOf[E].left, input) | treeCompare(pattern.asInstanceOf[E].right, input)
          case F(left, NIL()) => treeCompare(pattern.asInstanceOf[E].left, input.asInstanceOf[F].left) | treeCompare(pattern.asInstanceOf[E].right, input.asInstanceOf[F].left)
          case A(opt1, l, NIL()) =>  treeCompare(pattern.asInstanceOf[E].left, input) | treeCompare(pattern.asInstanceOf[E].right.asInstanceOf[E2].right, input)
          case _ => false
        }
      }
      case E(left, NIL()) => {
        input match{
          case E(lef, NIL()) => {
            treeCompare(pattern.asInstanceOf[E].left, input.asInstanceOf[E].left)
          }
          case T(lft, right:T) => treeCompare(pattern.asInstanceOf[E].left, input)
          case A(opt1, l, NIL()) => treeCompare(pattern.asInstanceOf[E].left, input)
          case _ => false
        }
      }
      case T(F(l, NIL()), T(left: MatchTree, right: MatchTree)) => {
        input match{
          case T(F(lft, NIL()), T(left: MatchTree, right: MatchTree)) => treeCompare(pattern.asInstanceOf[T].left, input.asInstanceOf[T].left) && treeCompare(pattern.asInstanceOf[T].right, input.asInstanceOf[T].right)
          case T(F(l, NIL()), NIL()) => false//treeCompare(pattern.asInstanceOf[T].left, input.asInstanceOf[T].left)
          case A(opt1, l, NIL()) => treeCompare(pattern.asInstanceOf[T].left, input)
          case _ => false
        }
      }
      case T(F(l, rt: F2), T(lf: MatchTree, r: MatchTree)) => {
        input match{
          case T(F(left, NIL()), r: T) => treeCompare(pattern.asInstanceOf[T].left.asInstanceOf[F].left, input.asInstanceOf[T].left.asInstanceOf[F].left) && treeCompare(pattern.asInstanceOf[T].right, input.asInstanceOf[T].right)
          case T(l: MatchTree, NIL()) => treeCompare(pattern.asInstanceOf[T].left.asInstanceOf[F].left, input.asInstanceOf[T].left.asInstanceOf[F].left) || treeCompare(pattern.asInstanceOf[T].right, input.asInstanceOf[T].left) || treeCompare(pattern.asInstanceOf[T].right, input)
          case A(opt1, l, NIL()) => treeCompare(pattern.asInstanceOf[T].left, input)
          case _ => false
        }
      }
      case T(F(l, rt: F2), NIL()) => {
        input match{
          case T(F(left, NIL()), T(lef: Any, right: T)) => treeCompare(pattern.asInstanceOf[T].left, input.asInstanceOf[T].left) && treeCompare(pattern.asInstanceOf[T].right, input.asInstanceOf[T].right)
          case T(F(left, NIL()), NIL()) =>  treeCompare(pattern.asInstanceOf[T].left.asInstanceOf[F].left, input.asInstanceOf[T].left.asInstanceOf[F].left)
          case A(opt1, l, NIL()) => treeCompare(pattern.asInstanceOf[T].left, input)
          case _ => false
        }
      }
      case T(l, NIL()) => {
        input match{
          case T(left, NIL()) => treeCompare(pattern.asInstanceOf[T].left, input.asInstanceOf[T].left)
          case T(left, right: MatchTree) => treeCompare(pattern.asInstanceOf[T].left, input)
          case A(opt1, l, NIL()) => treeCompare(pattern.asInstanceOf[T].left, input)
          case _ => false
        }
      }
      case F(l, NIL()) => {
        input match{
          case F(left, NIL()) => treeCompare(pattern.asInstanceOf[F].left, input.asInstanceOf[F].left)
          case T(left, right: T) => treeCompare(pattern.asInstanceOf[F].left, input)
          case T(lft, NIL()) => treeCompare(pattern.asInstanceOf[F].left, input)
          case A(opt1, l, NIL()) => treeCompare(pattern.asInstanceOf[F].left, input)
          case _ => false
        }
      }
      case A(C(s:String), "(", A2(l: Any, r: Any)) => {
        input match{
          case A(C(s: String), "", NIL()) => treeCompare(pattern.asInstanceOf[A].opt1, input.asInstanceOf[A].opt1) && treeCompare(pattern.asInstanceOf[A].opt2.asInstanceOf[A2].left, input)
        }
      }
      case A(C(s: String), "", NIL()) => {
        input match{
          case A(C(s: String), "", NIL()) => treeCompare(pattern.asInstanceOf[A].opt1, input.asInstanceOf[A].opt1)
          //case T(F(le: Any, ri: Any), T(lef: Any, rig: Any)) => treeCompare(pattern, input.asInstanceOf[T].left.asInstanceOf[F].left) || treeCompare(pattern, input.asInstanceOf[T].right)
          //case T(l: MatchTree, rig: NIL) => treeCompare(pattern, input.asInstanceOf[T].left.asInstanceOf[F].left)
          case _ => false
        }
      }
      case A(NIL(), "(", A2(left: Any, right: Any)) => treeCompare(pattern.asInstanceOf[A].opt2.asInstanceOf[A2].left, input)
      case C(ch) => {
        input match{
          case C(s:String) => {
            if(ch == "."){
              true
            } else {
              s == ch
            }
          }
          case _ => false
        }
      }
    }
  }
  
}

  
object Assignment extends Parser{
  def main(args: Array[String]){
    //println("pattern?")
    val pattern: String = readLine("pattern? ") 
    def patTree: MatchTree = parseAll(e, pattern).get
    var string = readLine("string? ")
    
    while (string != "quit"){
      def inTree: MatchTree = parseAll(e, string).get
      var matches: Boolean = treeCompare(patTree, inTree)
      if(matches) println("match") else println("no match")
      string = readLine("string? ")
    }
    //println("input : " + "hello world42")
    //println(parseAll(s, "((h|j)ell. worl?d)|(42)"))
    //def testPatTree: MatchTree = parseAll(e, "((h|j)ell. worl?d)|(42)").get
    //def testInTree: MatchTree = parseAll(e, "hello world").get
    //println(treeCompare(testPatTree, testInTree))
    //println("first")
    //recurseTree(testPatTree)
    //println("second")
    //recurseTree(testInTree)
    //println(parseAll(s, "24"))
    //def test: String = parseAll(s, "haskjfhkds").get.toString
    //println(test)
    //println(treeCompare("((h|j)ell. worl?d)|(42)", "hello world"))
    
  }
}

// (E2(|,E(T(F(A(C(b),,NIL()),NIL()),NIL()),NIL())),NIL())