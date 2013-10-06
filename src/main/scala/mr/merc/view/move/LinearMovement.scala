package mr.merc.view.move

// speed means pixels per second
class LinearMovement(x1:Int, y1:Int, x2:Int, y2:Int, speed:Int) extends Movement{
    private var timePassed = 0 // in ms
	
    def start() = {}
    
    def update(time:Int) {
      require(!isOver)
      timePassed += time
    }
    
    private val distance = scala.math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    private val timeToCover = distance * 1000 / speed toInt
    
    def coveredPart:Double = {
      val covered = timePassed.toDouble / timeToCover
      if (covered > 1) 1 else covered
    }
    
	def isOver:Boolean = timePassed >= timeToCover
    
    def x:Int = x1 + coveredPart * (x2 - x1) toInt
	def y:Int = y1 + coveredPart * (y2 - y1) toInt
 }