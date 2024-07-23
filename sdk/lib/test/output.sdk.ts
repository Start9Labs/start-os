import { StartSdk } from "../StartSdk"
import { setupManifest } from "../manifest/setupManifest"

export type Manifest = any
export const sdk = StartSdk.of()
  .withManifest(
    setupManifest({
      id: "testOutput",
      title: "",
      version: "1.0:0",
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
      images: {},
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
        "remote-test": {
          description: "",
          optional: false,
          s9pk: "https://example.com/remote-test.s9pk",
        },
      },
    }),
  )
  .withStore<{ storeRoot: { storeLeaf: "value" } }>()
  .build(true)
