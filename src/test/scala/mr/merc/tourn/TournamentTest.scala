package mr.merc.tourn

import org.scalatest.FunSuite

class TournamentTest extends FunSuite {
  val p1 = new Participant
  val p2 = new Participant
  val p3 = new Participant
  val p4 = new Participant
  val p5 = new Participant

  val participants = List(p1, p2, p3, p4)

  test("playoff round") {
    val playOff = new PlayOffTournamentRound(4, 2, _ => participants)
    assert(playOff.matchups === None)
    assert(playOff.matchupsInBestParts === None)
    playOff.init(participants)
    assert(playOff.matchups === Some(List(Matchup(p1, p2), Matchup(p3, p4))))
    assert(playOff.matchupsInBestParts === Some(List(List(Matchup(p1, p2), Matchup(p3, p4)))))
    assert(playOff.roundState.matchups === playOff.matchups)
  }

  test("roundRobin round") {
    val roundRobin = new RoundRobinTournamentRound(4, 2, 1, _ => participants)
    assert(roundRobin.matchups === None)
    assert(roundRobin.matchupsInBestParts === None)
    roundRobin.init(participants)
    assert(roundRobin.matchups === Some(List(Matchup(p1, p3), Matchup(p2, p4), Matchup(p1, p4), Matchup(p3, p2), Matchup(p1, p2), Matchup(p4, p3))))
    assert(roundRobin.matchupsInBestParts === Some(List(List(Matchup(p1, p3), Matchup(p2, p4)), List(Matchup(p1, p4), Matchup(p3, p2)), List(Matchup(p1, p2), Matchup(p4, p3)))))
  }

  test("roundRobin two groups, 3 tours") {
    val (p6, p7, p8) = (new Participant, new Participant, new Participant)
    val parts = List(p1, p2, p3, p4, p5, p6, p7, p8)
    val roundRobin = new RoundRobinTournamentRound(8, 4, 2, _ => parts)
    roundRobin.init(parts)
    assert(roundRobin.matchupsInBestParts === Some(List(
      List(Matchup(p1, p3), Matchup(p2, p4), Matchup(p5, p7), Matchup(p6, p8)),
      List(Matchup(p1, p4), Matchup(p3, p2), Matchup(p5, p8), Matchup(p7, p6)),
      List(Matchup(p1, p2), Matchup(p4, p3), Matchup(p5, p6), Matchup(p8, p7)))))

    assert(roundRobin.matchups === Some(List(Matchup(p1, p3), Matchup(p2, p4),
      Matchup(p1, p4), Matchup(p3, p2), Matchup(p1, p2), Matchup(p4, p3),
      Matchup(p5, p7), Matchup(p6, p8), Matchup(p5, p8), Matchup(p7, p6),
      Matchup(p5, p6), Matchup(p8, p7)
    )))
  }

  test("tournament integration test") {
    val roundRobin = new RoundRobinTournamentRound(4, 1)
    val playOff = new PlayOffTournamentRound(2, 1)
    val rounds = List(roundRobin, playOff)
    val ps = List(participants, List(p5))
    val tournament = new Tournament(rounds, ps)
    // start
    tournament.nextRoundIfPossible()
    assert(roundRobin.isStarted)
    assert(!roundRobin.isOver)
    assert(!playOff.isStarted)
    assert(!playOff.isOver)

    val matches = roundRobin.matchups.get
    assert(matches.size === 6)
    matches.foreach { m =>
      val gIndex = participants.indexOf(m.guest)
      val hIndex = participants.indexOf(m.host)
      if (gIndex < hIndex) {
        m.result = Some(MatchupResult(m.guest, m.host, 2, 0))
      } else if (hIndex < gIndex) {
        m.result = Some(MatchupResult(m.host, m.guest, 2, 0))
      } else {
        sys.error("impossible")
      }
    }

    assert(roundRobin.isStarted)
    assert(roundRobin.isOver)
    assert(!playOff.isStarted)
    assert(!playOff.isOver)

    def info(p:Participant) = roundRobin.roundState.calculateRoundRobinInfo.get(p)

    assert(info(p1) === RoundRobinInfo(3, 3, 0, 6, 0))
    assert(info(p2) === RoundRobinInfo(3, 2, 1, 4, 2))
    assert(info(p3) === RoundRobinInfo(3, 1, 2, 2, 4))
    assert(info(p4) === RoundRobinInfo(3, 0, 3, 0, 6))

    assert(roundRobin.roundState.participantsPlaces.get == participants)
    tournament.nextRoundIfPossible()

    assert(roundRobin.isStarted)
    assert(roundRobin.isOver)
    assert(playOff.isStarted)
    assert(!playOff.isOver)

    assert(playOff.participants.get.toSet === Set(p5, p1))
    val fin = playOff.matchups.get.head
    if (fin.host === p5) {
      fin.result = Some(MatchupResult(fin.host, fin.guest, 3, 1))
    } else {
      fin.result = Some(MatchupResult(fin.guest, fin.host, 3, 1))
    }
    assert(playOff.roundState.participantsPlaces === Some(List(p5, p1)))

    assert(roundRobin.isStarted)
    assert(roundRobin.isOver)
    assert(playOff.isStarted)
    assert(playOff.isOver)

    tournament.nextRoundIfPossible()

    assert(roundRobin.isStarted)
    assert(roundRobin.isOver)
    assert(playOff.isStarted)
    assert(playOff.isOver)
  }
}
