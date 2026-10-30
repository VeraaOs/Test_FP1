import java.io.File
import scala.sys.process.*

object Executor:
  // Process(…) — описание внешнего процесса:
  def runCommand(cmd: String, dir: String): (Int, String, String, Long) =
    val stdout = new StringBuilder // накопитель для обычного вывода команды
    val stderr = new StringBuilder // накопитель для сообщений об ошибках
    val t0 = System.currentTimeMillis()
    val exitCode = Process(
      Seq("/bin/bash", "-c", cmd),
      new File(dir)
    ).!(ProcessLogger(  // ! - запуск и ожидание
      line => { stdout.append(line).append("\n"); () }, // добавляем строку и перевод строки
      line => { stderr.append(line).append("\n"); () }
    ))
    val t1 = System.currentTimeMillis()
    (exitCode, stdout.toString.trim, stderr.toString.trim, t1 - t0) 

  def resolveCd(currentDir: String, target: String): Option[String] =
    val home = System.getProperty("user.home")
    val targetPath =  // вычисляем в зависимости от вида target
      if target == "~" then home
      else if target.startsWith("~/") then home + "/" + target.drop(2)
      else if target.startsWith("/") then target
      else currentDir + "/" + target
    val file = new File(targetPath).getCanonicalFile
    if file.isDirectory then Some(file.getAbsolutePath)
    else None