package fluent

import scala.io
import com.typesafe.config.ConfigFactory
import akka.actor._
import akka.cluster.Cluster
import akka.cluster.ClusterEvent._

object Printer {
  def props(): Props = {
    Props(new Printer())
  }
}

class Printer extends Actor {
  val cluster = Cluster(context.system)

  override def preStart(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent], classOf[UnreachableMember])
  }
  override def postStop(): Unit = cluster.unsubscribe(self)

  def receive = {
    case MemberUp(member) => {
      println("Member is Up: {}", member.address)
      println(">>> RootActorPath", RootActorPath(member.address) / "user" / "brother")
    }
    case UnreachableMember(member) =>
      println("Member detected as unreachable: {}", member)
    case MemberRemoved(member, previousStatus) =>
      println(
        "Member is Removed: {} after {}",
        member.address, previousStatus)
    case _: MemberEvent => // ignore
    case (s: String) => {
      println(s"[${self.path}] $s")
      if (self.path.name != "brother") {
        context.actorSelection("akka.tcp://actor_system@127.0.0.1:8001/user/brother") ! s
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val port = args(0)
    val name = args(1)
    val config = ConfigFactory.parseString(s"akka.remote.netty.tcp.port=$port").withFallback(ConfigFactory.load())
    val system = ActorSystem("actor_system", config)
    val printer = system.actorOf(Printer.props, name)

    val x = ConfigFactory.load()
    println(x.getValue("akka.cluster.seed-nodes"))

    print("ENTER TO SEND")
    io.StdIn.readLine()
    printer ! "world"

    print("ENTER TO KILL")
    io.StdIn.readLine()
    system.terminate
  }
}
