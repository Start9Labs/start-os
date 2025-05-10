import { StartSdk } from "../StartSdk"
import { setupManifest } from "../manifest/setupManifest"
import { VersionGraph } from "../version/VersionGraph"

export type Manifest = any
export const sdk = StartSdk.of()
  .withManifest(
    setupManifest({
      id: "testOutput",
      title: "",
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
      images: {
        main: {
          source: {
            dockerTag: "start9/hello-world",
          },
          arch: ["aarch64", "x86_64"],
          emulateMissingAs: "aarch64",
        },
      },
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
  .build(true)
