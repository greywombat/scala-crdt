package com.github.greywombat.crdt.mutable

import monix.reactive.Observable

trait MutableCRDT[+T, OpT, StateT] {
  def output: Observable[OpT]

  def update(input: OpT)

  def get(): T

  def state: StateT

  def merge(other: StateT)
}