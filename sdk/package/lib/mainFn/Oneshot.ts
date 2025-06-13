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

export class Oneshot<
  Manifest extends T.SDKManifest,
  C extends SubContainer<Manifest> | null = SubContainer<Manifest> | null,
> extends Daemon<Manifest, C> {
  static of<Manifest extends T.SDKManifest>() {
    return async <C extends SubContainer<Manifest> | null>(
      effects: T.Effects,
      subcontainer: C,
      exec: DaemonCommandType<Manifest, C>,
    ) => {
      let subc: SubContainer<Manifest> | null = subcontainer
      if (subcontainer && subcontainer.isOwned()) subc = subcontainer.rc()
      const startCommand = () =>
        CommandController.of<Manifest, C>()(
          effects,
          (subc?.rc() ?? null) as C,
          exec,
        )
      return new Oneshot<Manifest, C>(subcontainer, startCommand, true)
    }
  }
}
