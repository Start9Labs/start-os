import { ManifestVersion, SDKManifest } from "../../manifest/ManifestTypes"
import * as T from "../../types"

export class Migration<
  Manifest extends T.Manifest,
  Store,
  Version extends ManifestVersion,
> {
  constructor(
    readonly options: {
      version: Version
      up: (opts: { effects: T.Effects }) => Promise<void>
      down: (opts: { effects: T.Effects }) => Promise<void>
    },
  ) {}
  static of<
    Manifest extends T.Manifest,
    Store,
    Version extends ManifestVersion,
  >(options: {
    version: Version
    up: (opts: { effects: T.Effects }) => Promise<void>
    down: (opts: { effects: T.Effects }) => Promise<void>
  }) {
    return new Migration<Manifest, Store, Version>(options)
  }

  async up(opts: { effects: T.Effects }) {
    this.up(opts)
  }

  async down(opts: { effects: T.Effects }) {
    this.down(opts)
  }
}
