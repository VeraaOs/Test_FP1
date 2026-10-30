import scala.io.StdIn
import upickle.default.*
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

// зеркало рабочего класса Rule для чтения JSON
case class RuleJson(
                     id: String,
                     command: String,
                     priority: Int,
                     descriptionTemplate: String,
                     showCommand: Boolean,
                     outputMode: String,
                     maximumLines: Option[Int] = None,
                     enabled: Boolean
                   ) derives ReadWriter

// зеркало всего файла config.json.
case class ConfigJson(
                       shell: String,
                       reportDirectory: String,
                       sessionDirectory: String,
                       logFile: String,
                       defaultProfile: String,
                       rules: Seq[RuleJson]
                     ) derives ReadWriter

var session: Option[Session] = None
var currentDir: String = new File(".").getCanonicalPath
var skipNext: Boolean = false
var rules: Seq[Rule] = Seq()
var shellPath: String = "/bin/bash"
var reportDir: String = "reports"
var profileName: String = "linux-lab"

@main
def main(): Unit =
  loadConfig()
  println("report-shell запущен. Введите 'help' для списка команд.")
  var running = true
  while running do
    print("report-shell> ")
    val line = StdIn.readLine()
    if line == null then running = false
    else if line.trim.nonEmpty then
      running = handleLine(line.trim)

def handleLine(input: String): Boolean =
  val cmd = input.split("\\s+").head
  cmd match
    case "exit" | "quit" => false
    case "help" =>
      println("Служебные команды:")
      println("  starts \"Название\"  — начать сессию")
      println("  ends                — завершить сессию и сохранить отчёт")
      println("  prootd <путь>       — установить корневой каталог")
      println("  mstep \"Название\"    — начать новый шаг")
      println("  mnote \"Текст\"       — добавить заметку к шагу")
      println("  mskip               — исключить следующую команду из отчёта")
      println("  mstatus             — показать состояние сессии")
      println("  mpreview            — создать предварительный отчёт")
      println("  exit                — выйти из программы")
      true
    case "starts"   => startSession(input); true
    case "ends"     => endSession(); true
    case "mstep"    => addStep(input); true
    case "mnote"    => addNote(input); true
    case "mskip"    =>
      skipNext = true
      println("Следующая команда будет выполнена, но не попадёт в отчёт.")
      true
    case "mstatus"  => showStatus(); true
    case "prootd"   => setRoot(input); true
    case "mpreview" => preview(); true
    case _          => runOrdinary(input); true
// Достаёт текст в кавычках из ввода пользователя
def quotedArg(input: String, prefix: String): String =
  val raw = input.stripPrefix(prefix).trim
  if raw.startsWith("\"") && raw.endsWith("\"") && raw.length >= 2 then
    raw.substring(1, raw.length - 1)
  else raw
  
// Превращает название сессии в безопасное имя файла.
def slug(name: String): String =
  val s = name.toLowerCase.replaceAll("[^a-z0-9а-яё]+", "-").stripPrefix("-").stripSuffix("-")
  if s.isEmpty then "report" else s

def loadConfig(): Unit =
  try
    val text = new String(Files.readAllBytes(Paths.get("config.json")))
    val config = read[ConfigJson](text)
    shellPath = config.shell
    reportDir = config.reportDirectory
    profileName = config.defaultProfile
    rules = config.rules.map(r =>
      Rule(r.id, r.command, r.priority, r.descriptionTemplate,
        r.showCommand, r.outputMode, r.maximumLines, r.enabled)
    ).toSeq
    println(s"Конфигурация загружена: ${rules.size} правил, профиль '$profileName'.")
  catch
    case e: Exception =>
      println(s"Внимание: не удалось прочитать config.json: ${e.getMessage}")
      println("Будут использованы настройки по умолчанию.")

def startNew(name: String): Unit =
  session = Some(Session(
    name = name,
    startTime = Generator.nowString(),
    rootDir = currentDir,
    currentDir = currentDir,
    shell = shellPath,
    profile = profileName,
    steps = Seq()
  ))
  println(s"Сессия начата: $name")

def startSession(input: String): Unit =
  val name = quotedArg(input, "starts")
  val realName = if name.isEmpty then "Без названия" else name
  session match
    case Some(s) =>
      println(s"Уже есть активная сессия: ${s.name}")
      println("1) Продолжить её")
      println("2) Завершить и сформировать отчёт")
      println("3) Отменить незавершённую сессию")
      print("Ваш выбор: ")
      val choice = StdIn.readLine()
      choice match
        case "1" => println("Продолжаем текущую сессию.")
        case "2" => endSession(); startNew(realName)
        case "3" =>
          session = None
          println("Незавершённая сессия отменена.")
          startNew(realName)
        case _ => println("Неизвестный выбор. Оставлена текущая сессия.")
    case None => startNew(realName)

def ensureStep(): Unit =
  session = session.map { s =>
    if s.steps.isEmpty then s.copy(steps = Seq(Step("", Seq(), Seq())))
    else s
  }

def checkSession(s: Session): Seq[String] =
  var w = Seq.empty[String]
  if s.steps.isEmpty then w = w :+ "в сессии нет ни одного шага"
  else if s.steps.forall(st => st.commands.isEmpty) then w = w :+ "в сессии нет записанных команд"
  val grouped = rules.filter(r => r.enabled).groupBy(r => (r.command, r.priority))
  for ((_, group) <- grouped) do
    if group.size > 1 then
      w = w :+ s"конфликт правил для '${group.head.command}' с приоритетом ${group.head.priority}: ${group.map(g => g.id).mkString(", ")}"
  w

def endSession(): Unit =
  session match
    case Some(s) =>
      val warnings = checkSession(s)
      for w <- warnings do println(s"Предупреждение: $w")
      val report = Generator.generateReport(s, rules, warnings)
      val dir = new File(reportDir)
      dir.mkdirs()
      val file = new File(dir, slug(s.name) + ".md")
      val pw = new PrintWriter(file)
      pw.write(report)
      pw.close()
      println("Сессия завершена.")
      println(s"Отчёт сохранён: ${file.getPath}")
      session = None
    case None => println("Нет активной сессии.")

def addStep(input: String): Unit =
  session match
    case Some(s) =>
      val title = quotedArg(input, "mstep")
      session = Some(s.copy(steps = s.steps :+ Step(title, Seq(), Seq())))
      val shown = if title.isEmpty then s"Шаг ${s.steps.size + 1}" else title
      println(s"Новый шаг: $shown")
    case None => println("Сначала начните сессию: starts \"Название\"")

def addNote(input: String): Unit =
  session match
    case Some(_) =>
      ensureStep()
      val note = quotedArg(input, "mnote")
      session = session.map { s =>
        val last = s.steps.last
        val updated = last.copy(notes = last.notes :+ note)
        s.copy(steps = s.steps.updated(s.steps.size - 1, updated))
      }
      println("Заметка добавлена.")
    case None => println("Нет активной сессии.")

def showStatus(): Unit =
  session match
    case Some(s) =>
      val all = s.steps.flatMap(st => st.commands)
      println(s"Сессия: ${s.name}")
      println(s"Время начала: ${s.startTime}")
      println(s"Корневой каталог: ${s.rootDir}")
      println(s"Текущий каталог: $currentDir")
      val stepName = s.steps.lastOption match
        case Some(st) => if st.title.nonEmpty then st.title else s"Шаг ${s.steps.size}"
        case None => "(шага нет)"
      println(s"Текущий шаг: $stepName")
      println(s"Записано команд: ${all.size}")
      println(s"Исключено команд: ${all.count(c => !c.includeInReport)}")
    case None => println("Нет активной сессии.")

def setRoot(input: String): Unit =
  val path = input.stripPrefix("prootd").trim
  val target = if path.isEmpty || path == "." then currentDir else path
  Executor.resolveCd(currentDir, target) match
    case Some(abs) =>
      session = session.map(s => s.copy(rootDir = abs))
      println(s"Корневой каталог установлен: $abs")
    case None => println(s"Каталог не существует: $path")

def preview(): Unit =
  session match
    case Some(s) =>
      val warnings = checkSession(s)
      val report = Generator.generateReport(s, rules, warnings)
      val dir = new File(reportDir)
      dir.mkdirs()
      val file = new File(dir, "draft-" + slug(s.name) + ".md")
      val pw = new PrintWriter(file)
      pw.write(report)
      pw.close()
      println(s"Предварительный отчёт создан: ${file.getPath}")
    case None => println("Нет активной сессии.")

def warnIfOutsideRoot(): Unit =
  session match
    case Some(s) =>
      if !currentDir.startsWith(s.rootDir) then
        println("Предупреждение: текущий каталог находится вне корневого каталога сессии.")
        println("В отчёте будет использован абсолютный путь.")
    case None => ()

def runOrdinary(input: String): Unit =
  val cmd = input.split("\\s+").head
  if cmd == "cd" then
    val target = input.split("\\s+").lift(1).getOrElse("~")
    val oldDir = currentDir
    Executor.resolveCd(currentDir, target) match
      case Some(newDir) =>
        currentDir = newDir
        warnIfOutsideRoot()
        record(input, oldDir, newDir, 0, "", "", 0)
      case None => println(s"Каталог не существует: $target")
  else
    val start = Generator.nowString()
    val (code, out, err, duration) = Executor.runCommand(input, currentDir)
    val end = Generator.nowString()
    if out.nonEmpty then println(out)
    if err.nonEmpty then println(err)
    if code != 0 then println(s"Команда завершилась с кодом $code.")
    recordWithTimes(input, currentDir, currentDir, code, out, err, duration, start, end)

def record(text: String, before: String, after: String, code: Int, out: String, err: String, duration: Long): Unit =
  val t = Generator.nowString()
  recordWithTimes(text, before, after, code, out, err, duration, t, t)

// Ничего не меняется «на месте»: на каждом уровне создаётся НОВЫЙ объект:
def recordWithTimes(text: String, before: String, after: String, code: Int, out: String, err: String, duration: Long, start: String, end: String): Unit =
  session match
    case Some(_) =>
      ensureStep()
      val rec = CommandRecord(text, before, after, code, out, err, start, end, duration, !skipNext)
      skipNext = false
      session = session.map { s =>
        val last = s.steps.last
        val updated = last.copy(commands = last.commands :+ rec)
        s.copy(steps = s.steps.updated(s.steps.size - 1, updated), currentDir = currentDir)
      }
    case None => ()