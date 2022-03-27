package ru.dokwork.spellchecklsp

import java.util.{ HashMap, List => JList, Map => JMap }
import java.util.stream.{ Collector, Collectors, Stream => JStream, StreamSupport }

import org.eclipse.lsp4j.*

import com.google.common.collect.Streams
import org.languagetool.rules.RuleMatch
import java.util.Optional
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ListBuffer
import java.util.stream.Collector.Characteristics
import java.util.Arrays
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.collection.immutable.Range.Inclusive

/** Extends the [[scala.collection.Iterable]]
  */
extension [T <: Object](itr: scala.collection.Iterable[T])
  def stream: JStream[T] = Streams.stream(itr.asJava)

/** Extends the [[scala.Option]]
  */
extension [T](opt: Option[T])
  def stream: JStream[T]             = opt match
    case None        => JStream.empty
    case Some(value) => JStream.of(value)

/** Extends the [[scala.collection.Map]]
  */
extension [K, V](m: Map[K, V])
  def stream(key: K): JStream[V] =
    m.get(key).stream

  def stream: JStream[(K, V)] =
    Streams.stream(m.iterator.asJava)

/** Extends the [[java.util.Map]]
  */
extension [K, V](m: JMap[K, V])
  def stream(key: K): JStream[V]     =
    Optional.ofNullable(m.get(key)).stream

/** Extends the [[java.util.stream.Stream]]
  */
extension [T](s: JStream[T])
  def zipWithIndex: JStream[(T, Int)] =
    Streams.mapWithIndex(s, (t, i) => t -> i.toInt)

  def toJList: JList[T] = s.collect(Collectors.toList)

  def toJMap[K, V](using ev: T <:< (K, V)): JMap[K, V] =
    s.collect(() => HashMap[K, V](), (m, t) => m.put.tupled(ev(t)), (m, m1) => m.putAll(m1))

  def toIndexedSeq: IndexedSeq[T] =
    s.toJList.asScala.toIndexedSeq

/** Extends the [[org.eclipse.lsp4j.Position]]
  */
extension (pos: Position)
  def <=(other: Position): Boolean =
    pos.getLine <= other.getLine && pos.getCharacter <= other.getCharacter

  def >=(other: Position): Boolean =
    pos.getLine >= other.getLine && pos.getCharacter >= other.getCharacter

/** Extends the [[org.eclipse.lsp4j.Range]]
  */
extension (range: Range)
  def contains(pos: Position): Boolean =
    range.getStart <= pos && pos <= range.getEnd

  def includes(other: Range): Boolean =
    range.contains(other.getStart) && range.contains(other.getEnd)

  def intersectsWith(other: Range): Boolean =
    range.contains(other.getStart) || range.contains(other.getEnd)

  def lines: Inclusive = range.getStart.getLine to range.getEnd.getLine

/** Extends the [[org.eclipse.lsp4j.TextEdit]]
  */
extension (edit: TextEdit)
  def asAction(uri: Uri): CodeAction =
    val action = CodeAction(edit.getNewText)
    action.setEdit(WorkspaceEdit(JMap.of(uri.asString, JList.of(edit))))
    action
