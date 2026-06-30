import { i18n } from './i18n'
import { sdk } from './sdk'

export const main = sdk.setupMain(async ({ effects }) => {
  /**
   * ======================== Setup (optional) ========================
   *
   * Fetch any resources or run any preliminary commands here.
   */
  console.info(i18n('Starting {{name}}!'))

  /**
   * ======================== Daemons ========================
   *
   * Define one or more daemons that make up the service runtime. A daemon is
   * healthy as long as its process is running; the optional `ready` check below
   * lets you surface a more specific readiness state to the user.
   *
   * The ids here ('example-daemon', 'example-image', 'example-volume',
   * 'example-subcontainer') are arbitrary — rename them to suit your service.
   * 'example-image' must match an image key in startos/manifest/index.ts, and
   * 'example-volume' must match an entry in the manifest `volumes` array.
   */
  return sdk.Daemons.of(effects).addDaemon('example-daemon', {
    subcontainer: sdk.SubContainer.of(
      effects,
      { imageId: 'example-image' },
      sdk.Mounts.of().mountVolume({
        volumeId: 'example-volume',
        subpath: null,
        mountpoint: '/data',
        readonly: false,
      }),
      'example-subcontainer',
    ),
    exec: { command: ['hello-world'] },
    // Optional readiness check. Wire it up once the service exposes something to
    // check — e.g. an interface (see the Interfaces page in the guide). Add any
    // user-facing strings to startos/i18n/dictionaries before enabling:
    // ready: {
    //   display: i18n('Web Interface'),
    //   fn: () =>
    //     sdk.healthCheck.checkPortListening(effects, 80, {
    //       successMessage: i18n('The web interface is ready'),
    //       errorMessage: i18n('The web interface is not ready'),
    //     }),
    // },
    requires: [],
  })
})
