package examples.chat

object Api {
  case class Connect(dst: String, client_addr: String)
  case class MCast(dst: String, msg: String)
}
