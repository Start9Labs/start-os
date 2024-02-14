import { EmVer } from "../../emverLite/mod"
import { SDKManifest } from "../../manifest/ManifestTypes"
import { ExpectedExports } from "../../types"
import { createUtils } from "../../util"
import { once } from "../../util/once"
import { Migration } from "./Migration"

export class Migrations<Manifest extends SDKManifest, Store> {
  private constructor(
    readonly manifest: SDKManifest,
    readonly migrations: Array<Migration<Manifest, Store, any>>,
  ) {}
  private sortedMigrations = once(() => {
    const migrationsAsVersions = (
      this.migrations as Array<Migration<Manifest, Store, any>>
    ).map((x) => [EmVer.parse(x.options.version), x] as const)
    migrationsAsVersions.sort((a, b) => a[0].compareForSort(b[0]))
    return migrationsAsVersions
  })
  private currentVersion = once(() => EmVer.parse(this.manifest.version))
  static of<
    Manifest extends SDKManifest,
    Store,
    Migrations extends Array<Migration<Manifest, Store, any>>,
  >(manifest: SDKManifest, ...migrations: EnsureUniqueId<Migrations>) {
    return new Migrations(
      manifest,
      migrations as Array<Migration<Manifest, Store, any>>,
    )
  }
  async init({
    effects,
    previousVersion,
  }: Parameters<ExpectedExports.init>[0]) {
    const utils = createUtils<Manifest, Store>(effects)
    if (!!previousVersion) {
      const previousVersionEmVer = EmVer.parse(previousVersion)
      for (const [_, migration] of this.sortedMigrations()
        .filter((x) => x[0].greaterThan(previousVersionEmVer))
        .filter((x) => x[0].lessThanOrEqual(this.currentVersion()))) {
        await migration.up({ effects, utils })
      }
    }
  }
  async uninit({
    effects,
    nextVersion,
  }: Parameters<ExpectedExports.uninit>[0]) {
    const utils = createUtils<Manifest, Store>(effects)
    if (!!nextVersion) {
      const nextVersionEmVer = EmVer.parse(nextVersion)
      const reversed = [...this.sortedMigrations()].reverse()
      for (const [_, migration] of reversed
        .filter((x) => x[0].greaterThan(nextVersionEmVer))
        .filter((x) => x[0].lessThanOrEqual(this.currentVersion()))) {
        await migration.down({ effects, utils })
      }
    }
  }
}

export function setupMigrations<
  Manifest extends SDKManifest,
  Store,
  Migrations extends Array<Migration<Manifest, Store, any>>,
>(manifest: SDKManifest, ...migrations: EnsureUniqueId<Migrations>) {
  return Migrations.of<Manifest, Store, Migrations>(manifest, ...migrations)
}

// prettier-ignore
export type EnsureUniqueId<A, B = A, ids = never> =
  B extends [] ? A : 
  B extends [Migration<any, any, infer id>, ...infer Rest] ? (
    id extends ids ? "One of the ids are not unique"[] :
    EnsureUniqueId<A, Rest, id | ids>
  ) : "There exists a migration that is not a Migration"[]
