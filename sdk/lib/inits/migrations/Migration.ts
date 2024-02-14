import { ManifestVersion, SDKManifest } from "../../manifest/ManifestTypes"
import { Effects } from "../../types"
import { Utils } from "../../util/utils"

export class Migration<
  Manifest extends SDKManifest,
  Store,
  Version extends ManifestVersion,
> {
  constructor(
    readonly options: {
      version: Version
      up: (opts: {
        effects: Effects
        utils: Utils<Manifest, Store>
      }) => Promise<void>
      down: (opts: {
        effects: Effects
        utils: Utils<Manifest, Store>
      }) => Promise<void>
    },
  ) {}
  static of<
    Manifest extends SDKManifest,
    Store,
    Version extends ManifestVersion,
  >(options: {
    version: Version
    up: (opts: {
      effects: Effects
      utils: Utils<Manifest, Store>
    }) => Promise<void>
    down: (opts: {
      effects: Effects
      utils: Utils<Manifest, Store>
    }) => Promise<void>
  }) {
    return new Migration<Manifest, Store, Version>(options)
  }

  async up(opts: { effects: Effects; utils: Utils<Manifest, Store> }) {
    this.up(opts)
  }

  async down(opts: { effects: Effects; utils: Utils<Manifest, Store> }) {
    this.down(opts)
  }
}
