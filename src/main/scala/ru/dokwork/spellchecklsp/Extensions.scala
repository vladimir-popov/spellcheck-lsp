package ru.dokwork.spellchecklsp

import java.util.*
import java.util.stream.{ Collector, Collectors, Stream, StreamSupport }

import org.eclipse.lsp4j.*

import com.google.common.collect.Streams
import org.languagetool.rules.RuleMatch

extension [K, V](m: Map[K, V])
  def stream(key: K): Stream[V] =
    Optional.ofNullable(m.get(key)).stream

  def findValues(f: K => Boolean): Stream[V] =
    m.keySet.stream.filter(f(_)).flatMap(m.stream)

extension [T](s: Stream[T])
  def toList = s.collect(Collectors.toList)

  def zipWithIndex: Stream[(T, Int)] =
    Streams.mapWithIndex(s, (t, i) => t -> i.toInt)

extension [K, V](s: Stream[(K, V)])
  def toMap: Map[K, V] =
    s.collect(() => HashMap[K, V](), (m, t) => m.put(t._1, t._2), (m, m1) => m.putAll(m1))

extension (pos: Position)
  def <=(other: Position): Boolean =
    pos.getLine <= other.getLine && pos.getCharacter <= other.getCharacter

  def >=(other: Position): Boolean =
    pos.getLine >= other.getLine && pos.getCharacter >= other.getCharacter

extension (range: Range)
  def contains(pos: Position): Boolean =
    range.getStart <= pos && pos <= range.getEnd

  def contains(other: Range): Boolean =
    range.contains(other.getStart) && range.contains(other.getEnd)

extension (edit: TextEdit)
  def asAction(uri: String): CodeAction =
    val action = CodeAction(edit.getNewText)
    action.setEdit(
      WorkspaceEdit(Map.of(uri, List.of(edit)))
    )
    action
