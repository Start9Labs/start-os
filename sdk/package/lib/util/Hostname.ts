import { HostnameInfo } from "../../../base/lib/types"

export function hostnameInfoToAddress(hostInfo: HostnameInfo): string {
  if (hostInfo.kind === "onion") {
    return `${hostInfo.hostname.value}`
  }
  if (hostInfo.kind !== "ip") {
    throw Error("Expecting that the kind is ip.")
  }
  const hostname = hostInfo.hostname
  if (hostname.kind === "domain") {
    return `${hostname.subdomain ? `${hostname.subdomain}.` : ""}${hostname.domain}`
  }
  const port = hostname.sslPort || hostname.port
  const portString = port ? `:${port}` : ""
  if ("ipv4" === hostname.kind || "ipv6" === hostname.kind) {
    return `${hostname.value}${portString}`
  }
  if ("local" === hostname.kind) {
    return `${hostname.value}${portString}`
  }
  throw Error(
    "Expecting to have a valid hostname kind." + JSON.stringify(hostname),
  )
}
