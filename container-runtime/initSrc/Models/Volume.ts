import * as fs from "node:fs/promises"

export class Volume {
  constructor(
    readonly volumeId: string,
    readonly path = `/media/startos/volumes/${volumeId}`,
  ) {
    fs.stat(path)
  }
}
