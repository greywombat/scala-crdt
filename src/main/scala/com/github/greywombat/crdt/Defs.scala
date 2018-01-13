package com.github.greywombat.crdt

/**
  * Information to identify a client/agent/node/actor/process/whatever that accesses a CRDT concurrently.
  * You would typically set one such value implicitly for the context in which you write to a CRDT (e.g. per thread, JavaScript client, etc.).
  * In some CRDTs, the NodeId might be used to define a total order in the sequence of operations (e.g. for last-writer-wins semantics), thus the NodeId can have implications in determining which process wins a concurrent write operation.
  *
  * @param nodeId The actual node identifier.
  */
case class NodeId(nodeId: String)
