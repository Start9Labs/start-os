import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { NO_TIMEOUT, SIGTERM } from "../../../base/lib/types"

import * as T from "../../../base/lib/types"
import { SubContainer } from "../util/SubContainer"
import { Drop, splitCommand } from "../util"
import * as cp from "child_process"
import * as fs from "node:fs/promises"
import { DaemonCommandType, ExecCommandOptions, ExecFnOptions } from "./Daemons"

export class CommandController<
  Manifest extends T.SDKManifest,
  C extends SubContainer<Manifest> | null,
> extends Drop {
  private constructor(
    readonly runningAnswer: Promise<null>,
    private state: { exited: boolean },
    private readonly subcontainer: C,
    private process: cp.ChildProcess | AbortController,
    readonly sigtermTimeout: number = DEFAULT_SIGTERM_TIMEOUT,
  ) {
    super()
  }
  static of<
    Manifest extends T.SDKManifest,
    C extends SubContainer<Manifest> | null,
  >() {
    return async (
      effects: T.Effects,
      subcontainer: C,
      exec: DaemonCommandType<Manifest, C>,
    ) => {
      try {
        if ("fn" in exec) {
          const abort = new AbortController()
          const cell: { ctrl: CommandController<Manifest, C> } = {
            ctrl: new CommandController<Manifest, C>(
              exec.fn(subcontainer, abort).then(async (command) => {
                if (subcontainer && command && !abort.signal.aborted) {
                  const newCtrl = (
                    await CommandController.of<
                      Manifest,
                      SubContainer<Manifest>
                    >()(effects, subcontainer, command as ExecCommandOptions)
                  ).leak()

                  Object.assign(cell.ctrl, newCtrl)
                  return await cell.ctrl.runningAnswer
                } else {
                  cell.ctrl.state.exited = true
                }
                return null
              }),
              { exited: false },
              subcontainer,
              abort,
              exec.sigtermTimeout,
            ),
          }
          return cell.ctrl
        }
        let commands: string[]
        if (T.isUseEntrypoint(exec.command)) {
          const imageMeta: T.ImageMetadata = await fs
            .readFile(`/media/startos/images/${subcontainer!.imageId}.json`, {
              encoding: "utf8",
            })
            .catch(() => "{}")
            .then(JSON.parse)
          commands = imageMeta.entrypoint ?? []
          commands = commands.concat(
            ...(exec.command.overridCmd ?? imageMeta.cmd ?? []),
          )
        } else commands = splitCommand(exec.command)

        let childProcess: cp.ChildProcess
        if (exec.runAsInit) {
          childProcess = await subcontainer!.launch(commands, {
            env: exec.env,
          })
        } else {
          childProcess = await subcontainer!.spawn(commands, {
            env: exec.env,
            stdio: exec.onStdout || exec.onStderr ? "pipe" : "inherit",
          })
        }

        if (exec.onStdout) childProcess.stdout?.on("data", exec.onStdout)
        if (exec.onStderr) childProcess.stderr?.on("data", exec.onStderr)

        const state = { exited: false }
        const answer = new Promise<null>((resolve, reject) => {
          childProcess.on("exit", (code) => {
            state.exited = true
            if (
              code === 0 ||
              code === 143 ||
              (code === null && childProcess.signalCode == "SIGTERM")
            ) {
              return resolve(null)
            }
            if (code) {
              return reject(
                new Error(`${commands[0]} exited with code ${code}`),
              )
            } else {
              return reject(
                new Error(
                  `${commands[0]} exited with signal ${childProcess.signalCode}`,
                ),
              )
            }
          })
        })

        return new CommandController<Manifest, C>(
          answer,
          state,
          subcontainer,
          childProcess,
          exec.sigtermTimeout,
        )
      } catch (e) {
        await subcontainer?.destroy()
        throw e
      }
    }
  }
  async wait({ timeout = NO_TIMEOUT } = {}) {
    if (timeout > 0)
      setTimeout(() => {
        this.term()
      }, timeout)
    try {
      if (timeout > 0 && this.process instanceof AbortController)
        await Promise.race([
          this.runningAnswer,
          new Promise((_, reject) =>
            setTimeout(
              () =>
                reject(new Error("Timed out waiting for js command to exit")),
              timeout * 2,
            ),
          ),
        ])
      else await this.runningAnswer
    } finally {
      if (!this.state.exited) {
        if (this.process instanceof AbortController) this.process.abort()
        else this.process.kill("SIGKILL")
      }
      await this.subcontainer?.destroy()
    }
  }
  async term({ signal = SIGTERM, timeout = this.sigtermTimeout } = {}) {
    try {
      if (!this.state.exited) {
        if (this.process instanceof AbortController) return this.process.abort()

        if (signal !== "SIGKILL") {
          setTimeout(() => {
            if (this.process instanceof AbortController) this.process.abort()
            else this.process.kill("SIGKILL")
          }, timeout)
        }
        if (!this.process.kill(signal)) {
          console.error(
            `failed to send signal ${signal} to pid ${this.process.pid}`,
          )
        }
      }

      if (this.process instanceof AbortController)
        await Promise.race([
          this.runningAnswer,
          new Promise((_, reject) =>
            setTimeout(
              () =>
                reject(new Error("Timed out waiting for js command to exit")),
              timeout * 2,
            ),
          ),
        ])
      else await this.runningAnswer
    } finally {
      await this.subcontainer?.destroy()
    }
  }
  onDrop(): void {
    this.term().catch(console.error)
  }
}
