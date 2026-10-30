import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// чистые функции сборки Markdown-отчёта

object Generator:

  def nowString(): String =
    LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))

  def escapeInline(s: String): String =
    s.replace("\\", "\\\\").replace("`", "\\`").replace("*", "\\*").replace("_", "\\_")

  def fenceFor(content: String): String =
    var fence = "```"
    while content.contains(fence) do fence += "`"
    fence

  def formatDuration(ms: Long): String =
    if ms < 1000 then s"$ms мс" else f"${ms / 1000.0}%.1f с"

  def relativePath(session: Session, dir: String): String =
    if dir.startsWith(session.rootDir) then
      dir.stripPrefix(session.rootDir).stripPrefix("/")
    else dir

  def fillTemplate(template: String, values: Map[String, String]): String =
    var result = template
    for ((key, value) <- values) do
      result = result.replace("{" + key + "}", value)
    result

  def findRule(rules: Seq[Rule], commandName: String): Option[Rule] =
    rules.filter(r => r.enabled).filter(r => r.command == commandName)
      .sortBy(r => -r.priority).headOption
      
  // Режимы включения вывода
  def outputBlock(rec: CommandRecord, mode: String, maxLines: Option[Int]): String =
    val limit = maxLines.getOrElse(10)
    val lines = if rec.stdout.isEmpty then Seq.empty[String] else rec.stdout.split("\n").toSeq
    def whole: String =
      val fence = fenceFor(rec.stdout)
      s"\n$fence\n${rec.stdout}\n$fence\n"
    def truncated(shown: Seq[String], label: String): String =
      val fence = fenceFor(shown.mkString("\n"))
      s"\n$fence\n${shown.mkString("\n")}\n$fence\n\n*Показаны $label ${shown.size} строк из ${lines.size}.*\n"
    mode match
      case "full" => if lines.isEmpty then "" else whole
      case "errors-only" =>
        if rec.stderr.nonEmpty then
          val fence = fenceFor(rec.stderr)
          s"\n$fence\n${rec.stderr}\n$fence\n"
        else ""
      case "none" => ""
      case "first-lines" | "limited" =>
        if lines.size <= limit then whole else truncated(lines.take(limit), "первые")
      case "last-lines" =>
        if lines.size <= limit then whole else truncated(lines.takeRight(limit), "последние")
      case "summary" => s"\nВывод: ${lines.size} строк, код завершения: `${rec.exitCode}`.\n"
      case _ => ""
      
  // Подстановка значений в шаблоны
  def commandToMarkdown(rec: CommandRecord, rule: Option[Rule], session: Session, stepTitle: String): String =
    val parts = rec.text.split(" ").toSeq
    val values = Map(
      "command" -> rec.text,
      "commandName" -> parts.headOption.getOrElse(""),
      "arguments" -> parts.drop(1).mkString(" "),
      "argument1" -> parts.lift(1).getOrElse(""),
      "argument2" -> parts.lift(2).getOrElse(""),
      "currentDirectory" -> rec.directoryAfter,
      "relativeDirectory" -> relativePath(session, rec.directoryAfter),
      "exitCode" -> rec.exitCode.toString,
      "duration" -> formatDuration(rec.durationMillis),
      "stepTitle" -> stepTitle
    )
    rule match
      case Some(r) =>
        val desc = fillTemplate(r.descriptionTemplate, values)
        val cmdBlock =
          if r.showCommand then
            val fence = fenceFor(rec.text)
            s"\nКоманда:\n$fence bash\n${rec.text}\n$fence\n"
          else ""
        desc + cmdBlock + outputBlock(rec, r.outputMode, r.maximumLines)
      case None =>
        val fence = fenceFor(rec.text)
        val base = s"Выполним команду:\n$fence bash\n${rec.text}\n$fence\n"
        val out =
          if rec.stdout.nonEmpty then
            val f2 = fenceFor(rec.stdout)
            s"\nРезультат:\n$f2\n${rec.stdout}\n$f2\n"
          else ""
        val err =
          if rec.exitCode != 0 then
            val errBlock =
              if rec.stderr.nonEmpty then
                val f3 = fenceFor(rec.stderr)
                s"\n$f3\n${rec.stderr}\n$f3\n"
              else ""
            s"\nКод завершения: `${rec.exitCode}`." + errBlock + "\n"
          else ""
        base + out + err

  def generateReport(session: Session, rules: Seq[Rule], warnings: Seq[String]): String =
    val sb = new StringBuilder
    sb.append(s"# ${session.name}\n\n")
    sb.append("## Сведения о сессии\n\n")
    sb.append(s"- Начало: ${session.startTime}\n")
    sb.append(s"- Завершение: ${nowString()}\n")
    sb.append(s"- Корневой каталог: `${session.rootDir}`\n")
    sb.append(s"- Командный интерпретатор: `${session.shell}`\n")
    sb.append(s"- Профиль отчёта: `${session.profile}`\n")
    sb.append(s"- Количество шагов: ${session.steps.count(st => st.commands.nonEmpty || st.notes.nonEmpty)}\n\n")
    for ((step, i) <- session.steps.zipWithIndex) do
      val shown = step.commands.filter(c => c.includeInReport)
      if shown.nonEmpty || step.notes.nonEmpty then
        val heading =
          if step.title.isEmpty then s"## Шаг ${i + 1}"
          else s"## Шаг ${i + 1}. ${step.title}"
        sb.append(heading + "\n\n")
        for note <- step.notes do
          sb.append(escapeInline(note)).append("\n\n")
        for rec <- shown do
          val rule = findRule(rules, rec.text.split(" ").headOption.getOrElse(""))
          sb.append(commandToMarkdown(rec, rule, session, step.title))
          sb.append("\n")
    val failed = session.steps.flatMap(st => st.commands).filter(c => c.exitCode != 0)
    if warnings.nonEmpty || failed.nonEmpty then
      sb.append("## Ошибки и предупреждения\n\n")
      for w <- warnings do sb.append(s"- Предупреждение: $w\n")
      for c <- failed do sb.append(s"- Команда `${c.text}` завершилась с кодом ${c.exitCode}.\n")
      sb.append("\n")
    val all = session.steps.flatMap(s => s.commands)
    val total = all.size
    val ok = all.count(c => c.exitCode == 0)
    val skipped = all.count(c => !c.includeInReport)
    sb.append("## Итоги\n\n")
    sb.append(s"- Выполнено команд: $total\n")
    sb.append(s"- Успешно: $ok\n")
    sb.append(s"- Завершилось с ошибкой: ${total - ok}\n")
    sb.append(s"- Исключено из отчёта: $skipped\n")
    sb.toString