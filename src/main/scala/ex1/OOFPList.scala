package ex1

import scala.annotation.tailrec
import scala.language.postfixOps

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A]) //:: è un costruttore, utile per creare una lista (non corrisponde a Cons, ma è un "metodo" di creazione)
  case Nil()
  def ::(h: A): List[A] = List.::(h, this) //il metodo crea una lista richiamando il costruttore

  //restituisce la testa della lista
  def head: Option[A] = this match
    case h :: t => Some(h) //sintassi sostitutiva a Cons(h, t), che indica l'esistenza di una lista
    case _ => None

  //restituisce la coda della lista
  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  //esegue una funzione void su tutti gli elementi della lista
  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  //restituisce l'elemento della lista in una determinata posizione
  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  //compie un'operazione da sinistra verso destra sugli elementi della lista (init è il valore con cui il primo elemento viene sommato)
  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op) //prendo di volta in volta il risultato della funzione e lo eseguo con la testa
    case _ => init

  //compie un'operazione da destra verso sinistra sugli elementi della lista (init è il valore con cui l'ultimo elemento viene sommato)
  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op)) //prendo di volta in volta il risultato della funzione e lo eseguo con la testa, a ritroso
    case _ => init

  //concatena due liste
  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  //compie un'operazione da sinistra verso destra sugli elementi della lista (simile a foldLeft) con la differenza che non viene deciso il valore init
  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)


  def zipWithValue[B](value: B): List[(A, B)] = this match
    case head :: tail => (head, value) :: tail.zipWithValue(value)
    case _ => Nil()

  def zipWithValue2[B](value: B): List[(A, B)] =
    //affinchè sia possibile utilizzare foldRight, devo specificare una funzione con lo stesso numero di parametri richiesti da foldRight. In particolare, ai fini del calcolo, mi è utile specificare:
    def combine(head: A, acc: (List[(A, B)], B)): (List[(A, B)], B) = //head è l'elemento della lista; List[(...)] è la lista da generare man mano e B rappresenta il valore sempre uguale da raggruppare con head (devono essere per forza in questo ordine)
      ((head, value) :: acc._1, value) //restituisco la coppia formata da: (lista aggiornata, valore comune)

    foldRight((List[(A, B)](), value))(combine)._1 //alla fine restituisco solo il primo elemento di ogni tupla (lista(head, value), value) ottenuta dall'applicazione della funzione su ogni head (a combine vengono automaticamente passati gli argomenti)

  def length(): Int =
    @tailrec
    def doLength(acc: Int, list: List[A]): Int = list match
      case head :: tail => doLength(acc + 1, tail)
      case _ => acc

    doLength(0, this)

  def length2(): Int = foldLeft(0)((acc, head) => acc + 1)

  def zipWithIndex: List[(A, Int)] =
    def doZip(acc: Int)(list : List[A]) : List[(A, Int)] = list match
      case head :: tail => (head, acc) :: doZip(acc + 1)(tail)
      case _ => Nil()

    doZip(0)(this)

  def zipWithIndex2: List[(A, Int)] =
    def combine(acc: (List[(A, Int)], Int), head: A): (List[(A, Int)], Int) =
      ((head, acc._2) :: acc._1, acc._2 + 1)

    foldLeft((List[(A, Int)](), 0))(combine)._1

  def partition(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def doPartition(listTrue: List[A], listFalse: List[A], myList: List[A]) : (List[A], List[A]) = myList match
      case head :: tail =>
        if predicate(head) then
          doPartition(head :: listTrue, listFalse, tail)
        else
          doPartition(listTrue, head :: listFalse, tail)
      case _ => (listTrue, listFalse)

    doPartition(List[A](), List[A](), this)

  def partition2(predicate: A => Boolean): (List[A], List[A]) =
    def doPartition(head: A, acc: (List[A], List[A])): (List[A], List[A]) =
      if predicate(head) then
        (head :: acc._1, acc._2)
      else
        (acc._1, head :: acc._2)

    foldRight(List[A](), List[A]())(doPartition)

  //si ferma al primo elemento che non rispetta il predicato anche se nella lista esistono elementi successivi che rispettano il predicato
  def span(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def doSpan(list1: List[A], list2: List[A], myList: List[A]): (List[A], List[A]) = myList match
      case head :: tail =>
        if predicate(head) && list2.length() == 0 then
          doSpan(head :: list1, list2, tail)
        else
          doSpan(list1, head :: list2, tail)
      case _ => (list1, list2)

    doSpan(List[A](), List[A](), this)

  def span2(predicate: A => Boolean): (List[A], List[A]) =
    def doSpan(acc: (List[A], List[A], Int), head: A): (List[A], List[A], Int) =
      if predicate(head) && acc._3 == 0 then
        (head :: acc._1, acc._2, acc._3)
      else
        (acc._1, head :: acc._2, acc._3 + 1)

    foldLeft(List[A](), List[A](), 0)(doSpan) match
      case (list1, list2, _) => (list1, list2)

  def takeRight(n: Int): List[A] =
    @tailrec
    def doTakeRight(index: Int, list: List[A], newList: List[A]): List[A] = list match
      case head :: tail =>
        if index >= length() - n then {
          doTakeRight(index, tail, head :: newList)
        } else
          doTakeRight(index + 1, tail, newList)
      case _ => newList

    doTakeRight(0, this, List[A]())

  def takeRight2(n: Int): List[A] =
    def doTakeRight(acc: (List[A], Int), head: A): (List[A], Int) =
      if acc._2 >= length() - n then
        (head :: acc._1, acc._2 + 1)
      else
        (acc._1, acc._2 + 1)

    foldLeft(List[A](), 0)(doTakeRight)._1

  def collect(predicate: PartialFunction[A, A]): List[A] = this match
    case head :: tail =>
      if predicate.isDefinedAt(head) then
        predicate(head) :: tail.collect(predicate)
      else
        tail.collect(predicate)
    case _ => Nil()

  def collect2(predicate: PartialFunction[A, A]): List[A] =
    def doCollect(head: A, acc: (List[A], Option[A])): (List[A], Option[A]) =
      if predicate.isDefinedAt(head) then
        (predicate(head) :: acc._1, None)
      else
        (acc._1, None)

    foldRight(List[A](), None)(doCollect)._1


object List:

  def apply[A](elems: A*): List[A] = //A* significa che apply può accettare 0 o più argomenti come parametri
    var list: List[A] = Nil()
    for e <- elems.reverse
      do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:

  import List.*
  val reference = List(1, 2, 3, 4)
  println(reference.zipWithValue2(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.zipWithIndex2) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition2(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span2(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span2(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight2(3)) // List(2, 3, 4)
  println(reference.collect2{ case x if x % 2 == 0 => x + 1 }) // List(3, 5)