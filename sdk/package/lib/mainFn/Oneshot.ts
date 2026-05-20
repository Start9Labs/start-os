import * as T from '../../../base/lib/types'
import { SubContainer } from '../util/SubContainer'
import { CommandController } from './CommandController'
import { Daemon } from './Daemon'
import { DaemonCommandType } from './Daemons'

/**
 * A one-shot command: same machinery as a {@link Daemon} but exits after a
 * successful run instead of restarting.
 */
export class Oneshot<
  Manifest extends T.SDKManifest,
  C extends SubContainer<Manifest> | null = SubContainer<Manifest> | null,
> extends Daemon<Manifest, C> {
  static of<Manifest extends T.SDKManifest>() {
    return <C extends SubContainer<Manifest> | null>(
      effects: T.Effects,
      subcontainer: C,
      exec: DaemonCommandType<Manifest, C>,
    ) => {
      const startCommand = () =>
        CommandController.of<Manifest, C>()(effects, subcontainer, exec)
      return new Oneshot<Manifest, C>(subcontainer, startCommand, true)
    }
  }
}
