package examples.chat

import fluent._
import fluent.Rule.Infix

class Server(val host: String, val port: Int) extends FluentProgram {
  val connect = new Channel[(String, String)](
    "connect", List("server_addr", "client_addr"))
  val mcast = new Channel[(String, String)](
    "mcast", List("addr", "msg"))
  val nodelist = new Table[Tuple1[String]](
    "nodelist", List("addr"))

  override val name = "chat_server"

  override val collections = List(connect, mcast, nodelist)

  override val rules = {
    val rule1 = nodelist += Relation(connect).map(t => Tuple1(t._2))
    val rule2 = mcast += Relation(mcast).
      cross(Relation(nodelist)).
      map(t => (t._2._1, t._1._2))
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
