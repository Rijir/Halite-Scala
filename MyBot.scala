/**
  * Implementation of Halite bot by Timothy Trindle
  * Created by snoe on 7/23/16.
  */
import scala.collection.immutable
import java.util.ArrayList

class MyBot(id: Int, gameMapField:GameMap) extends HaliteBot(id, gameMapField) {
  final val STRENGTH_TO_EXTEND = 0

  override def name = "RijirBot"

  override def takeTurn(turn:BigInt, gameMap:GameMap): MoveList = {
    val moves = new MoveList()
    for (y <- 0 to gameMap.height - 1) {
      for (x <- 0 to gameMap.width - 1) {
        val loc: Location = new Location(x, y)
        val site: Site = gameMap.getSite(loc)
        if (site.owner == id) {
          val dir: Direction =
            if (site.strength == 0)
              Direction.STILL
            else if (isInternalTile(gameMap, loc, id))
              directionToEdge(gameMap, loc, id)
            else if (site.strength > STRENGTH_TO_EXTEND) {
              Direction.CARDINALS.find(d => {
                                         val siteOther: Site = gameMap.getSite(loc, d)
                                         siteOther.owner != id && siteOther.strength < site.strength
                                       })
                match {
                case Some(d) => d
                case _ => Direction.STILL
              }
            }
            else
              Direction.STILL

          moves.add(new Move(loc, dir))
        }
      }
    }
    moves
  }

  def isInternalTile(gameMap: GameMap, loc: Location, id: Int) = {
    Direction.CARDINALS.forall(dir => gameMap.getSite(loc, dir).owner == id)
  }

  class Distance (val d: Double) extends Ordered[Distance] {
    override def compare(that: Distance) = {
      (d - that.d).toInt
    }
  }

  def directionToEdge(gameMap: GameMap, loc: Location, id: Int) = {
    val locations: List[Location] =
      (0 until gameMap.height).foldLeft[List[Location]](List.empty[Location]) ((locations: List[Location], y: Int) =>
        locations ++ (0 until gameMap.width).map((x: Int) =>
          new Location(x, y)))
    val edgeLocations: Iterable[Location] = locations.filter((l: Location) =>
        gameMap.getSite(l).owner != id)

    val closestEdge: Option[Location] =
      Util.argMin(edgeLocations, (l: Location) =>
        new Distance(gameMap.getDistance(loc, l)))
    val angleTowardsEdge = closestEdge match {
      case Some(closest) => gameMap.getAngle(loc, closest)
      case None => 0
    }
    angleToDirection(angleTowardsEdge)
  }

  def angleToDirection(theta: Double) = {
    if (theta >= -Math.PI / 4 && theta < Math.PI / 4)
      Direction.WEST
    else if (theta >= Math.PI / 4 && theta < 3 * Math.PI / 4)
      Direction.SOUTH
    else if (theta >= -3 * Math.PI / 4 && theta < -Math.PI / 4)
      Direction.NORTH
    else // theta < -3 * Math.PI / 4 || theta >= 3 * Math.PI / 4
      Direction.EAST
  }
}

object MyBot {

  def main(args:Array[String]):Unit = {

    val maker = new HaliteBotMaker() {
      override def makeBot(id:Int, gameMap:GameMap):HaliteBot = new MyBot(id, gameMap)
    }

    HaliteBot.run(args, maker)
  }
}
