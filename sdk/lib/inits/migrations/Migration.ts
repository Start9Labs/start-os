import { ManifestVersion, SDKManifest } from "../../manifest/ManifestTypes"
import { Effects } from "../../types"

export class Migration<
  Manifest extends SDKManifest,
  Store,
  Version extends ManifestVersion,
> {
  constructor(
    readonly options: {
      version: Version
      up: (opts: { effects: Effects }) => Promise<void>
      down: (opts: { effects: Effects }) => Promise<void>
    },
  ) {}
  static of<
    Manifest extends SDKManifest,
    Store,
    Version extends ManifestVersion,
  >(options: {
    version: Version
    up: (opts: { effects: Effects }) => Promise<void>
    down: (opts: { effects: Effects }) => Promise<void>
  }) {
    return new Migration<Manifest, Store, Version>(options)
  }

  async up(opts: { effects: Effects }) {
    this.up(opts)
  }

  async down(opts: { effects: Effects }) {
    this.down(opts)
  }
}
