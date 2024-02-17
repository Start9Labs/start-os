import { StartSdk } from "../StartSdk"
import { setupManifest } from "../manifest/setupManifest"

export type Manifest = any
export const sdk = StartSdk.of()
  .withManifest(
    setupManifest({
      id: "testOutput",
      title: "",
      version: "1.0",
      releaseNotes: "",
      license: "",
      replaces: [],
      wrapperRepo: "",
      upstreamRepo: "",
      supportSite: "",
      marketingSite: "",
      donationUrl: null,
      description: {
        short: "",
        long: "",
      },
      containers: {},
      images: [],
      volumes: [],
      assets: [],
      alerts: {
        install: null,
        update: null,
        uninstall: null,
        restore: null,
        start: null,
        stop: null,
      },
      dependencies: {
        remoteTest: {
          description: "",
          requirement: { how: "", type: "opt-in" },
          version: "1.0",
        },
      },
    }),
  )
  .withStore<{ storeRoot: { storeLeaf: "value" } }>()
  .build(true)
