import * as fs from "node:fs/promises"

export class Volume {
  readonly path: string
  constructor(
    readonly volumeId: string,
    _path = "",
  ) {
    const path = (this.path = `/media/startos/volumes/${volumeId}${
      !_path ? "" : `/${_path}`
    }`)
  }
  async exists() {
    return fs.stat(this.path).then(
      () => true,
      () => false,
    )
  }
}
