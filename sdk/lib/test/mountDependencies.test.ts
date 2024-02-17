import { setupManifest } from "../manifest/setupManifest"
import { mountDependencies } from "../dependency/mountDependencies"
import {
  BuildPath,
  setupDependencyMounts,
} from "../dependency/setupDependencyMounts"

describe("mountDependencies", () => {
  const clnManifest = setupManifest({
    id: "cln",
    title: "",
    version: "1",
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
    assets: [],
    images: [],
    volumes: ["main"],
    alerts: {
      install: null,
      update: null,
      uninstall: null,
      restore: null,
      start: null,
      stop: null,
    },
    dependencies: {},
  })
  const clnManifestVolumes = clnManifest.volumes
  const lndManifest = setupManifest({
    id: "lnd",
    title: "",
    version: "1",
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
    assets: [],
    images: [],
    volumes: ["main2"],
    alerts: {
      install: null,
      update: null,
      uninstall: null,
      restore: null,
      start: null,
      stop: null,
    },
    dependencies: {},
  })
  clnManifest.id

  test("Types work", () => {
    const dependencyMounts = setupDependencyMounts()
      .addPath({
        name: "root",
        volume: "main",
        path: "/",
        manifest: clnManifest,
        readonly: true,
      })
      .addPath({
        name: "root",
        manifest: lndManifest,
        volume: "main2",
        path: "/",
        readonly: true,
      })
      .addPath({
        name: "root",
        manifest: lndManifest,
        // @ts-expect-error Expect that main will throw because it is not in the thing
        volume: "main",
        path: "/",
        readonly: true,
      })
      .build()
    ;() => {
      const test = mountDependencies(
        null as any,
        dependencyMounts,
      ) satisfies Promise<{
        cln: {
          main: {
            root: string
          }
        }
        lnd: {
          main2: {
            root: string
          }
        }
      }>
      const test2 = mountDependencies(
        null as any,
        dependencyMounts.cln,
      ) satisfies Promise<{
        main: { root: string }
      }>
      const test3 = mountDependencies(
        null as any,
        dependencyMounts.cln.main,
      ) satisfies Promise<{
        root: string
      }>
    }
  })
})
