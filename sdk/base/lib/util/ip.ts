export class IpAddress {
  readonly octets: number[]
  constructor(readonly address: string) {
    if (address.includes(":")) {
      this.octets = new Array(16).fill(0)
      const segs = address.split(":")
      let idx = 0
      let octIdx = 0
      while (segs[idx]) {
        const num = parseInt(segs[idx], 16)
        this.octets[octIdx++] = num >> 8
        this.octets[octIdx++] = num & 255
        idx += 1
      }
      const lastSegIdx = segs.length - 1
      if (idx < lastSegIdx) {
        idx = lastSegIdx
        octIdx = 15
        while (segs[idx]) {
          const num = parseInt(segs[idx], 16)
          this.octets[octIdx--] = num & 255
          this.octets[octIdx--] = num >> 8
          idx -= 1
        }
      }
    } else {
      this.octets = address.split(".").map(Number)
      if (this.octets.length !== 4) throw new Error("invalid ipv4 address")
    }
    if (this.octets.some((o) => o >= 256)) {
      throw new Error("invalid ip address")
    }
  }
  static parse(address: string): IpAddress {
    return new IpAddress(address)
  }
  isIpv4(): boolean {
    return this.octets.length === 4
  }
  isIpv6(): boolean {
    return this.octets.length === 16
  }
  isPublic(): boolean {
    return this.isIpv4() && !PRIVATE_IPV4_RANGES.some((r) => r.contains(this))
  }
}

export class IpNet extends IpAddress {
  readonly prefix
  constructor(readonly ipnet: string) {
    const [address, prefixStr] = ipnet.split("/", 2)
    super(address)
    this.prefix = Number(prefixStr)
  }
  static parse(ipnet: string): IpNet {
    return new IpNet(ipnet)
  }
  contains(address: string | IpAddress): boolean {
    if (typeof address === "string") address = new IpAddress(address)
    if (this.octets.length !== address.octets.length) return false
    let prefix = this.prefix
    let idx = 0
    while (idx < this.octets.length && prefix >= 8) {
      if (this.octets[idx] !== address.octets[idx]) {
        return false
      }
      idx += 1
      prefix -= 8
    }
    if (prefix === 0 || idx >= this.octets.length) return true
    const mask = 255 << prefix
    return (this.octets[idx] & mask) === (address.octets[idx] & mask)
  }
}

export const PRIVATE_IPV4_RANGES = [
  new IpNet("127.0.0.0/8"),
  new IpNet("10.0.0.0/8"),
  new IpNet("172.16.0.0/12"),
  new IpNet("192.168.0.0/16"),
]

export const IPV6_LINK_LOCAL = new IpNet("fe80::/10")

export const CGNAT = new IpNet("100.64.0.0/10")
