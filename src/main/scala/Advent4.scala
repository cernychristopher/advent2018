import scala.collection.mutable
import scala.io.Source

object Advent4 {
  private val Days = List(0,31,28,31,30,31,30,31,31,30,31,30,31)
  private val DateRegex = """\[1518-(\d\d)-(\d\d) (\d\d):(\d\d)\]""".r.unanchored
  private val AsleepRegex = "falls asleep".r.unanchored
  private val WakeupRegex = "wakes up".r.unanchored
  private val GuardRegex = """Guard #(\d+) begins shift""".r.unanchored

  case class Date(day: Int, minute: Int)

  sealed trait Event { def date: Date }
  type GuardId = Int
  case class Begin(guard: GuardId, date: Date) extends Event
  case class Sleep(date: Date) extends Event
  case class Wake(date: Date) extends Event

  def parseDate(in: String): Date = in match {
    case DateRegex(month, day, hour, minute) =>
      val dayOfYear = Days.take(month.toInt).sum + day.toInt
      if (hour.toInt == 23)
        Date(dayOfYear + 1, 0)
      else
        Date(dayOfYear, hour.toInt * 60 + minute.toInt)
  }

  def parseEvent(in: String): Event = {
    val date = parseDate(in)
    in match {
      case AsleepRegex() => Sleep(date)
      case WakeupRegex() => Wake(date)
      case GuardRegex(id) => Begin(id.toInt, date)
    }
  }

  def computeEventsByGuard(events: List[Event]): Map[GuardId, List[Event]] = {
    val m = mutable.HashMap[GuardId, mutable.ListBuffer[Event]]()
    var guard:Option[GuardId] = None

    events.foreach { event: Event =>
      event match {
        case Begin(id, _) => guard = Some(id)
        case _ =>
      }
      m.getOrElseUpdate(guard.get, mutable.ListBuffer()) += event
    }

    m.mapValues(_.toList).toMap
  }

  def main(args: Array[String]): Unit = {

    val testInput = Source.fromURL(getClass.getResource("/4.input")).getLines().toList.filterNot(_.isEmpty).sorted
    val eventsByGuard = computeEventsByGuard(testInput.map(parseEvent))
    val sleepTimes = eventsByGuard.map { case (guardId, events) =>
        guardId -> events.filter {
          case _:Begin => false
          case _ => true
        }.grouped(2).map {
          case Sleep(sleepDate) :: Wake(wakeDate) :: Nil => sleepDate.minute until wakeDate.minute
        }.toList
    }
    val totalSleepTimes = sleepTimes.mapValues(_.map(_.size).sum)
    val longestSleeper = totalSleepTimes.maxBy(_._2)
    println(longestSleeper)
    val sleepInMinutes = sleepTimes.mapValues { ranges =>
      ranges.map(_.groupBy(identity).mapValues(_ => 1)).fold[Map[Int, Int]](Map.empty) { case (a, b) =>
        (a.toList ++ b.toList).groupBy(_._1).mapValues(_.map(_._2).sum)
      }
    }
    val longestSleepMinutes = sleepInMinutes.filter(_._2.nonEmpty).mapValues(_.maxBy(_._2))

    val longestSleepMinute = longestSleepMinutes(longestSleeper._1)
    val solution1 = longestSleeper._1 * longestSleepMinute._1
    println(solution1)

    val topSleeper = longestSleepMinutes.maxBy(_._2._2)
    println(topSleeper)
    val solution2 = topSleeper._1 * topSleeper._2._1
    println(solution2)
  }
}
