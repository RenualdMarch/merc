package mr.merc.politics


object Government {

  sealed abstract class AbsolutismLevel
  case object Absolute extends AbsolutismLevel
  case object Constitutional extends AbsolutismLevel
  case object Democratic extends AbsolutismLevel

  sealed abstract class GovernmentType(val absolutismLevel: AbsolutismLevel)

  // aristocrats and magical aristocracy rule
  case object AbsoluteMonarchy extends GovernmentType(Absolute)

  // clergy and aristocrats rules
  case object AbsoluteTheocracy extends GovernmentType(Absolute)

  // mages and magical aristocracy rule
  case object AbsoluteMagocracy extends GovernmentType(Absolute)

  // aristocrats and magical aristocracy and capitalists rule, limiting power of the king
  case object ConstitutionalMonarchy extends GovernmentType(Constitutional)

  // mages and magical aristocracy rule, limiting power of the king
  case object ConstitutionalMagocracy extends GovernmentType(Constitutional)

  // clergy and aristocrats rule, limiting power of the king
  case object ConstitutionalTheocracy extends GovernmentType(Constitutional)

  // all people rule, severly limiting power of king
  case object Democracy extends GovernmentType(Democratic)

  // all people rule, mages are forbidden to be citizens
  case object EqualistsDemocracy extends GovernmentType(Democratic)

  // all people vote, but capitalists vote more
  case object OligarchicDemocracy extends GovernmentType(Democratic)

}