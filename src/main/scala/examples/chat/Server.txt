package examples.chat

import fluent._

class Server(val host: String, val port: Int) extends FluentProgram {
  import Api._
  case class NodeList(addr: String)

  val connect = new Channel[Connect]("connect")
  val mcast = new Channel[MCast]("mcast")
  val nodelist = new SetUnionLattice[NodeList]()

  override val name = "chat_server"

  override val rules = {
    List[Rule](
      nodelist += connect.map(c => NodeList(c.client_addr)),
      mcast += mcast.cross(nodelist).map({case (m, n) => MCast(n.addr, m.msg)})
    )
  }
}

object Server {
  def main(args: Array[String]) = {
    if (args.length != 2) {
      println("Server <host> <port>")
      System.exit(1)
    }
    val host = args(0)
    val port = args(1).toInt
    val server = new Server(host, port)
    println(s"This program is monotonic? ${server.isMonotonic()}")
    println(s"This program is increasing? ${server.isIncreasing()}")
    val (system, actor) = server.run()
  }
}
