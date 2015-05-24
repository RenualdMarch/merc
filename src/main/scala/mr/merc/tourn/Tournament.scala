package mr.merc.tourn

import scala.util.Random

class Tournament(val rounds: List[TournamentRound], val joiningParticipants: List[List[Participant]]) {

  def nextRoundIfPossible(): Unit = {
    val notStartedRoundOpt = rounds.find(r => !r.isStarted)
    val notFinishedRoundOpt = rounds.find(r => !r.isOver)

    /* possible cases -
        there is round that is not started, and it is the same that is not over
           this means we should start it
        there is round that is not started, and another round that is not over
           this means that tournament is in progress
        we can't find round that is not started, but can find that is not over
           this means that tournament is in progress
        we can't find anything - tournament is over
    */

    (notStartedRoundOpt, notFinishedRoundOpt) match {
      case (Some(notStarted), Some(notFinished)) => if (notStarted == notFinished) {
        val index = rounds.indexOf(notStarted)
        val joining = joiningParticipants.lift(index).getOrElse(Nil)
        val fromPrevRound = if (index > 0) rounds(index - 1).roundState.output.get
        else Nil
        notStarted.init(joining ++ fromPrevRound)
      } // else no need to do anything
      case (Some(notStarted), None) => sys.error("Impossible case")
      case (None, Some(notFinished)) => // last round in progress, do nothing
      case (None, None) => // tournament is over, do nothing
    }

  }
}

case class Matchup(host: Participant, guest: Participant, var result: Option[MatchupResult] = None)

case class MatchupResult(winner: Participant, loser: Participant, winnerPoints:Int, loserPoints:Int) {}

abstract class TournamentRoundState {
  def parent: TournamentRound
  // zero index is 1, then 1 is 2, and so on
  final def participantsPlaces: Option[List[Participant]] = if (parent.isOver) {
    Some(calculatePlaces(parent.matchups.get, parent.participants.get))
  } else {
    None
  }

  def calculatePlaces(matches: List[Matchup], participants: List[Participant]): List[Participant]

  def outputNumber: Int

  final def output: Option[List[Participant]] = participantsPlaces.map(_.take(outputNumber))
}

abstract class TournamentRound(val inputParticipantsCount: Int, val outputParticipantsCount: Int) {
  require(outputParticipantsCount < inputParticipantsCount, s"output $outputParticipantsCount less then input $inputParticipantsCount")

  private var _matchups: Option[List[Matchup]] = None
  def matchups = _matchups
  def roundState:TournamentRoundState
  def matchupsInBestParts = matchups.map(divideMatchups)

  def participants = _participants
  private var _participants:Option[List[Participant]] = None

  def generateMatchups(participants: List[Participant]):List[Matchup]

  def divideMatchups(m:List[Matchup]):List[List[Matchup]]

  def init(participants: List[Participant]): Unit = {
    require(_participants.isEmpty, "participants are already set")
    require(participants.size == inputParticipantsCount, s"must be $inputParticipantsCount participants, but was ${participants.size}")

    _participants = Some(participants)
    _matchups = Some(generateMatchups(participants))
  }

  def isOver = matchups.exists(_.forall(_.result.isDefined))

  def isStarted = participants.isDefined
}

class PlayOffTournamentRound(inputParticipantsCount: Int, outputParticipantsCount: Int, randomizer: Function[List[Participant], List[Participant]] = DefaultRandomizer)
  extends TournamentRound(inputParticipantsCount, outputParticipantsCount) {
  require(inputParticipantsCount / 2 == outputParticipantsCount,
    s"input is $inputParticipantsCount, output is $outputParticipantsCount, not possible to create playoff game")
  require(inputParticipantsCount % 2 == 0, s"not possible to create playoff with odd number $inputParticipantsCount")

  override def generateMatchups(participants: List[Participant]):List[Matchup] = {
    participants.grouped(2).map { l =>
      val List(x, y) = l
      Matchup(x, y)
    }.toList
  }

  override def divideMatchups(m:List[Matchup]) = List(m)

  def roundState = new PlayOffTournamentRoundState(this)
}

class PlayOffTournamentRoundState(val parent: PlayOffTournamentRound) extends TournamentRoundState {
  def matchups = parent.matchups

  def outputNumber = parent.outputParticipantsCount

  def calculatePlaces(matches: List[Matchup], participants: List[Participant]): List[Participant] = {
    val winners = matches.map(_.result.get.winner)
    val losers = matches.map(_.result.get.loser)
    winners ::: losers
  }

}

class RoundRobinTournamentRound(inputParticipantsCount: Int, outputParticipantsCount: Int, groupsCount: Int = 1,
                                randomizer: Function[List[Participant], List[Participant]] = DefaultRandomizer)
  extends TournamentRound(inputParticipantsCount, outputParticipantsCount) {
  require(inputParticipantsCount % 2 == 0, "input must be even number")
  require(inputParticipantsCount % groupsCount == 0, "Can't divide participants between groups")
  require((inputParticipantsCount / groupsCount) % 2 == 0, "Groups are with odd number of participants")

  private var _groups: Option[List[List[Participant]]] = None
  def groups = _groups


  override def generateMatchups(participants: List[Participant]):List[Matchup] = {
    val parts = randomizer(participants)
    val groupSize = inputParticipantsCount / groupsCount
    _groups = Some(parts.grouped(groupSize).toList)

    _groups.get.flatMap { parts =>
      val first = parts.head
      val others = parts.tail
      val tours = for (i <- 0 until groupSize - 1) yield {
        val head = others.take(i)
        val tail = others.drop(i)
        val list = first :: tail ::: head
        val hosts = list.take(list.size / 2)
        val guests = list.drop(list.size / 2)
        hosts zip guests map {case (h, g) => Matchup(h, g)}
      }
      tours.flatten.toList
    }
  }

  override def divideMatchups(m:List[Matchup]):List[List[Matchup]] = {
    // matchups are grouped in a way that all matchups group 1 than all matchups group 2 and so on
    val gamesPerGroupPerTour = inputParticipantsCount / (2 * groupsCount)
    val divided = m.grouped(gamesPerGroupPerTour).toList // 1 tour 1 group, 2 tour 1 group, 3 tour 1 group, 1 tour 2 group, ...
    val tours = inputParticipantsCount / groupsCount - 1
    val result = for (t <- 0 until tours) yield {
      val tourForGroups = for (g <- 0 until groupsCount) yield {
        divided(g * tours + t)
      }
      tourForGroups.toList.flatten
    }
    result.toList
  }

  def roundState = new RoundRobinTournamentRoundState(this)
}


case class RoundRobinInfo(matches: Int, wins: Int, loses: Int, pointsScored: Int, pointsScoredAgainst: Int)

class RoundRobinTournamentRoundState(val parent: RoundRobinTournamentRound) extends TournamentRoundState {
  def outputNumber = parent.outputParticipantsCount

  def calculatePlaces(matches: List[Matchup], participants: List[Participant]): List[Participant] = {
    val results = calculateRoundRobinInfo.get.toList
    val grouped = results.groupBy{case (p, _) => parent.groups.get.indexWhere(_.contains(p))}.toList.sortBy(_._1)
    val sortedGroups = grouped.map{case (_, l) => l.sortBy{case (_, r) => (r.wins, r.pointsScored - r.pointsScoredAgainst, r.pointsScored) }}
    sortedGroups.transpose.flatMap(_.map(_._1)).reverse
  }

  def calculateRoundRobinInfo:Option[Map[Participant, RoundRobinInfo]] = {
    parent.participants.map { parts =>
      val map = parts.map(p => (p, RoundRobinInfo(0, 0, 0, 0, 0))).toMap
      parent.matchups.map { m =>
        m.foldLeft(map) { case(mp, matchup) =>
          matchup.result.map{ result =>
            val w = mp(result.winner)
            val winnersResultAfter = RoundRobinInfo(w.matches + 1, w.wins + 1, w.loses, w.pointsScored + result.winnerPoints, w.pointsScoredAgainst + result.loserPoints)
            val l = mp(result.loser)
            val losersResultAfter = RoundRobinInfo(l.matches + 1, l.wins, l.loses + 1, l.pointsScored + result.loserPoints, l.pointsScoredAgainst + result.winnerPoints)
            mp ++ Map(result.winner -> winnersResultAfter, result.loser -> losersResultAfter)
          }.getOrElse(mp)
        }
      }.getOrElse(map)
    }
  }

}

object DefaultRandomizer extends Function[List[Participant], List[Participant]] {
  def apply(input:List[Participant]) = Random.shuffle(input)
}