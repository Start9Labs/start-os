import { ExtendedVersion } from "../../exver"

import * as T from "../../types"
import { once } from "../../util/once"
import { Migration } from "./Migration"

export class Migrations<Manifest extends T.Manifest, Store> {
  private constructor(
    readonly manifest: T.Manifest,
    readonly migrations: Array<Migration<Manifest, Store, any>>,
  ) {}
  private sortedMigrations = once(() => {
    const migrationsAsVersions = (
      this.migrations as Array<Migration<Manifest, Store, any>>
    )
      .map((x) => [ExtendedVersion.parse(x.options.version), x] as const)
      .filter(([v, _]) => v.flavor === this.currentVersion().flavor)
    migrationsAsVersions.sort((a, b) => a[0].compareForSort(b[0]))
    return migrationsAsVersions
  })
  private currentVersion = once(() =>
    ExtendedVersion.parse(this.manifest.version),
  )
  static of<
    Manifest extends T.Manifest,
    Store,
    Migrations extends Array<Migration<Manifest, Store, any>>,
  >(manifest: T.Manifest, ...migrations: EnsureUniqueId<Migrations>) {
    return new Migrations(
      manifest,
      migrations as Array<Migration<Manifest, Store, any>>,
    )
  }
  async init({
    effects,
    previousVersion,
  }: Parameters<T.ExpectedExports.init>[0]) {
    if (!!previousVersion) {
      const previousVersionExVer = ExtendedVersion.parse(previousVersion)
      for (const [_, migration] of this.sortedMigrations()
        .filter((x) => x[0].greaterThan(previousVersionExVer))
        .filter((x) => x[0].lessThanOrEqual(this.currentVersion()))) {
        await migration.up({ effects })
      }
    }
  }
  async uninit({
    effects,
    nextVersion,
  }: Parameters<T.ExpectedExports.uninit>[0]) {
    if (!!nextVersion) {
      const nextVersionExVer = ExtendedVersion.parse(nextVersion)
      const reversed = [...this.sortedMigrations()].reverse()
      for (const [_, migration] of reversed
        .filter((x) => x[0].greaterThan(nextVersionExVer))
        .filter((x) => x[0].lessThanOrEqual(this.currentVersion()))) {
        await migration.down({ effects })
      }
    }
  }
}

export function setupMigrations<
  Manifest extends T.Manifest,
  Store,
  Migrations extends Array<Migration<Manifest, Store, any>>,
>(manifest: T.Manifest, ...migrations: EnsureUniqueId<Migrations>) {
  return Migrations.of<Manifest, Store, Migrations>(manifest, ...migrations)
}

// prettier-ignore
export type EnsureUniqueId<A, B = A, ids = never> =
  B extends [] ? A : 
  B extends [Migration<any, any, infer id>, ...infer Rest] ? (
    id extends ids ? "One of the ids are not unique"[] :
    EnsureUniqueId<A, Rest, id | ids>
  ) : "There exists a migration that is not a Migration"[]
