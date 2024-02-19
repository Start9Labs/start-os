import { AddressInfo } from "../types"
import { Host, BindOptions } from "./Host"

export class Origin<T extends Host> {
  constructor(
    readonly host: T,
    readonly options: BindOptions,
  ) {}

  build({ username, path, search }: BuildOptions): AddressInfo {
    const qpEntries = Object.entries(search)
      .map(
        ([key, val]) => `${encodeURIComponent(key)}=${encodeURIComponent(val)}`,
      )
      .join("&")

    const qp = qpEntries.length ? `?${qpEntries}` : ""

    return {
      hostId: this.host.options.id,
      options: this.options,
      suffix: `${path}${qp}`,
      username,
    }
  }
}

type BuildOptions = {
  scheme: string | null
  username: string | null
  path: string
  search: Record<string, string>
}
