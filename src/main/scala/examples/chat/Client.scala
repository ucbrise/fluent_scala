package examples.chat

import fluent._

class Client(
    val server_host: String,
    val server_port: Int,
    val name: String,
    val host: String,
    val port: Int)
  extends FluentProgram {

  import Api._
  val stdout = new StdOut()
  val connect = new Channel[Connect]("connect")
  val mcast = new Channel[MCast]("mcast")

  override val bootstrap_rules = {
    import SetUnionLattice._
    List[fluent.Rule](
      connect += Val(Connect(s"$server_host:$server_port", hostport)),
      mcast += Val(MCast(s"$server_host:$server_port", name))
    )
  }

  override val rules = {
    List[fluent.Rule](stdout += mcast.map(_.msg))
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
    println(s"This program is increasing? ${client.isIncreasing()}")
    val (system, actor) = client.run()
  }
}
