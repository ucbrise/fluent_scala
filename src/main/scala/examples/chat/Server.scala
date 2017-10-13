package examples.chat

import fluent._
import fluent.Rule.Infix

class Server(val host: String, val port: Int) extends FluentProgram {
  import Api._
  case class NodeList(addr: String)

  val connect = new Channel[Connect]("connect")
  val mcast = new Channel[MCast]("mcast")
  val nodelist = new Table[NodeList]("nodelist")

  override val name = "chat_server"

  override val collections = List(connect, mcast, nodelist)

  override val rules = {
    val rule1 = {
      nodelist += connect.map(c => NodeList(c.client_addr))
    }
    val rule2 = {
      mcast += mcast.
               cross(Relation(nodelist)).
               map({case (m, n) => MCast(n.addr, m.msg)})
    }
    List(rule1, rule2)
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
    val (system, actor) = server.run()
  }
}
