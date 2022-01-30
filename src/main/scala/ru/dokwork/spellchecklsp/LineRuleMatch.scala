package ru.dokwork.spellchecklsp

import java.util.stream.Stream

import org.eclipse.lsp4j.*

import org.languagetool.rules.RuleMatch
import org.languagetool.rules.RuleMatch.Type

case class LineRuleMatch(rule: RuleMatch, lineNumber: Int):
  def getRange: Range =
    val start = Position(lineNumber, rule.getFromPos)
    val end   = Position(lineNumber, rule.getToPos)
    Range(start, end)

  def asDiagnostic: Diagnostic =
    val start    = Position(lineNumber, rule.getFromPos)
    val end      = Position(lineNumber, rule.getToPos)
    val range    = Range(start, end)
    val severity = rule.getType match
      case Type.UnknownWord => DiagnosticSeverity.Warning
      case Type.Hint        => DiagnosticSeverity.Hint
      case _                => DiagnosticSeverity.Information

    Diagnostic(range, rule.getMessage, severity, "spellchecklsp")

  def asTextChanges: Stream[TextEdit] =
    rule.getSuggestedReplacements.stream.map(TextEdit(getRange, _))
