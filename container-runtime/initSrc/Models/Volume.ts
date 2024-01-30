import * as fs from "node:fs"

export class Volume {
  readonly path: string
  constructor(readonly volumeId: string, _path = "") {
    const path = (this.path = `/media/startos/volumes/${volumeId}${
      !_path ? "" : `/${_path}`
    }`)
    fs.statSync(path)
  }
}
