package examples.chat

import fluent._
import fluent.Rule.Infix

class Client(
    val server_host: String,
    val server_port: Int,
    val name: String,
    val host: String,
    val port: Int)
  extends FluentProgram {

  val stdout = new Stdout()
  val connect = new Channel[(String, String)](
    "connect", List("server_addr", "client_addr"))
  val mcast = new Channel[(String, String)](
    "mcast", List("addr", "msg"))

  override val collections = List(stdout, connect, mcast)

  override val bootstrap_rules = {
    List(
      connect += Const((s"$server_host:$server_port", hostport)),
      mcast += Const((s"$server_host:$server_port", name)),
    )
  }

  override val rules = {
    List(stdout += Relation(mcast).map(t => Tuple1(t._2)))
  }
}

object Client {
  def main(args: Array[String]) = {
    if (args.length != 5) {
      println("Server <server_host> <server_port> <name> <host> <port>")
      System.exit(1)
    }
    val server_host = args(0)
    val server_port = args(1).toInt
    val name = args(2)
    val host = args(3)
    val port = args(4).toInt
    val client = new Client(server_host, server_port, name, host, port)
    println(s"This program is monotonic? ${client.isMonotonic()}")
    val (system, actor) = client.run()
  }
}
