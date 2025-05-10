import * as T from "../../../base/lib/types"
import { SubContainer } from "../util/SubContainer"
import { CommandController } from "./CommandController"
import { Daemon } from "./Daemon"

/**
 * This is a wrapper around CommandController that has a state of off, where the command shouldn't be running
 * and the others state of running, where it will keep a living running command
 * unlike Daemon, does not restart on success
 */

export class Oneshot<Manifest extends T.SDKManifest> extends Daemon<Manifest> {
  static of<Manifest extends T.SDKManifest>() {
    return async (
      effects: T.Effects,
      subcontainer: SubContainer<Manifest>,
      command: T.CommandType,
      options: {
        env?:
          | {
              [variable: string]: string
            }
          | undefined
        cwd?: string | undefined
        user?: string | undefined
        onStdout?: (chunk: Buffer | string | any) => void
        onStderr?: (chunk: Buffer | string | any) => void
        sigtermTimeout?: number
      },
    ) => {
      const startCommand = () =>
        CommandController.of<Manifest>()(
          effects,
          subcontainer,
          command,
          options,
        )
      return new Oneshot(startCommand, true, [])
    }
  }

  onExitSuccess(fn: () => void) {
    if (this.exitedSuccess) {
      fn()
    } else {
      this.onExitSuccessFns.push(fn)
    }
  }
}
