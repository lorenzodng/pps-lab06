package ex2

import ex2.Question.{Confidence, Final, Relevance}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//trait è un'interfaccia
trait ConferenceReviewing:
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Integer]
  def averageFinalScore(article: Int): Double
  def acceptedArticles: mutable.Set[Int]
  def sortedAcceptedArticles: ListBuffer[(Int, Double)]
  def averageWeightedFinalScoreMap(article: Int): Map[Int, Double]

end ConferenceReviewing

//enum può essere vista come un tipo di interfaccia in cui sono definiti tutti gli elementi utili
enum Question:
  case Relevance
  case Significance
  case Confidence
  case Final

end Question

class ConferenceReviewingImpl extends  ConferenceReviewing {

  private val reviews: ListBuffer[(Integer, mutable.Map[Question, Integer])] = ListBuffer()

  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    val scores: mutable.Map[Question, Integer] = mutable.Map()
    scores.put(Question.Relevance, relevance)
    scores.put(Question.Significance, significance)
    scores.put(Question.Confidence, confidence)
    scores.put(Question.Final, fin)
    reviews.addOne(article, scores)

  override def orderedScores(article: Int, question: Question): List[Integer] =
    reviews.filter(el => el._1 == article).flatMap(el => el._2.get(question)).sorted.toList //filtro sull'articolo e accedo al valore della chiave con flatmap o map (flatmap mi da i risultati in modo più pulito, riportando solo i numeri, ma è uguale a map)

  override def averageFinalScore(article: Int): Double =
    val finalScores = reviews.filter(el => el._1 == article).flatMap(el => el._2.get(Final))
    var sum = 0
    for score <- finalScores do
      sum = sum + score
    sum / finalScores.size

  override def acceptedArticles: mutable.Set[Int] =
    val articles = mutable.Set[Int]()
    for item <- reviews do
      if averageFinalScore(item._1) >= 5 && checkRelevanceScore(item._1) then
        articles.addOne(item._1)

    articles

  private def checkRelevanceScore(article: Int): Boolean =
    var check = false;
    val relevanceScores = reviews.filter(el => el._1 == article).flatMap(el => el._2.get(Relevance)).toList
    relevanceScores.exists(_ >= 8)

  override def sortedAcceptedArticles: ListBuffer[(Int, Double)] =
    val sortedArticles = ListBuffer[(Int, Double)]()
    for article <- acceptedArticles do
      sortedArticles.addOne(article, averageFinalScore(article))

    sortedArticles.sortBy(_._2)

  override def averageWeightedFinalScoreMap(article : Int): Map[Int, Double] =
    val scores = reviews.filter(el => el._1 == article).flatMap(el =>
      for
        confidence <- el._2.get(Confidence)
        relevance <- el._2.get(Final)
      yield (confidence, relevance)).toList
    var averageWeighted = 0
    for item <- scores do
      averageWeighted = averageWeighted + ((item._1 * item._2) / 10)
    val totalAverageWeighted = (averageWeighted / scores.size).toDouble
    Map(article -> totalAverageWeighted)
}






