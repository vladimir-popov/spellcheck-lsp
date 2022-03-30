package ru.dokwork.spellchecklsp

import java.util.stream.{ Stream => JStream }

import org.eclipse.lsp4j.*

import org.languagetool.rules.RuleMatch
import org.languagetool.rules.RuleMatch.Type



/** One suggested grammar rule.
  *
  * @param range
  *   range inside the document in which the rule can be applied. Always includes only single line.
  */
case class Suggestion(range: Range, rule: RuleMatch):

  def asDiagnostic: Diagnostic =
    val severity = rule.getType match
      case Type.UnknownWord => DiagnosticSeverity.Warning
      case Type.Hint        => DiagnosticSeverity.Hint
      case _                => DiagnosticSeverity.Information
    Diagnostic(range, rule.getMessage, severity, "spellchecklsp")

  def asTextEdits: JStream[TextEdit] =
    rule.getSuggestedReplacements.stream.map(TextEdit(range, _))
