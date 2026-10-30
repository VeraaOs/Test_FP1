case class CommandRecord(
                          text: String,
                          directoryBefore: String,
                          directoryAfter: String,
                          exitCode: Int,
                          stdout: String,
                          stderr: String,
                          startTime: String,
                          endTime: String,
                          durationMillis: Long,
                          includeInReport: Boolean
                        )

case class Step(
                 title: String,
                 notes: Seq[String],
                 commands: Seq[CommandRecord]
               )

case class Session(
                    name: String,
                    startTime: String,
                    rootDir: String,
                    currentDir: String,
                    shell: String,
                    profile: String,
                    steps: Seq[Step]
                  )

// правило генерации отчёта, читается из config.json
case class Rule(
                 id: String,
                 command: String,
                 priority: Int,
                 descriptionTemplate: String,
                 showCommand: Boolean,
                 outputMode: String,
                 maximumLines: Option[Int],
                 enabled: Boolean
               )