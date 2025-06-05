import * as T from "../../../base/lib/types"
import { SubContainer, SubContainerOwned } from "../util/SubContainer"
import { CommandController } from "./CommandController"
import { Daemon } from "./Daemon"
import { DaemonCommandType } from "./Daemons"

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
      exec: DaemonCommandType | null,
    ) => {
      if (subcontainer.isOwned()) subcontainer = subcontainer.rc()
      const startCommand = exec
        ? () =>
            CommandController.of<Manifest>()(effects, subcontainer.rc(), exec)
        : null
      return new Oneshot(subcontainer, startCommand, true)
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
