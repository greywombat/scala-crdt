package com.github.greywombat.crdt

import java.util.UUID

/**
  * Information to identify a client/agent/node/actor/process/whatever that accesses a CRDT concurrently.
  * You would typically set one such value implicitly for the context in which you write to a CRDT (e.g. per thread, JavaScript client, etc.).
  * In some CRDTs, the NodeId might be used to define a total order in the sequence of operations (e.g. for last-writer-wins semantics), thus the NodeId can have implications in determining which process wins a concurrent write operation.
  *
  * @param nodeId The actual node identifier.
  */
case class NodeId(nodeId: String) extends Ordered[NodeId] {
  override def compare(that: NodeId): Int = nodeId.compare(that.nodeId)
}

object UniqueId {
  def apply(): UniqueId = apply(UUID.randomUUID().toString)
}

case class UniqueId(uuid: String) extends Ordered[UniqueId] {
  override def compare(that: UniqueId): Int = uuid.compare(that.uuid)
}
