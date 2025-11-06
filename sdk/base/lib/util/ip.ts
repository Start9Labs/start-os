export class IpAddress {
  protected constructor(
    readonly octets: number[],
    readonly address: string,
  ) {}
  static parse(address: string): IpAddress {
    let octets
    if (address.includes(":")) {
      octets = new Array(16).fill(0)
      const segs = address.split(":")
      let idx = 0
      let octIdx = 0
      while (segs[idx]) {
        const num = parseInt(segs[idx], 16)
        octets[octIdx++] = num >> 8
        octets[octIdx++] = num & 255
        idx += 1
      }
      const lastSegIdx = segs.length - 1
      if (idx < lastSegIdx) {
        idx = lastSegIdx
        octIdx = 15
        while (segs[idx]) {
          const num = parseInt(segs[idx], 16)
          octets[octIdx--] = num & 255
          octets[octIdx--] = num >> 8
          idx -= 1
        }
      }
    } else {
      octets = address.split(".").map(Number)
      if (octets.length !== 4) throw new Error("invalid ipv4 address")
    }
    if (octets.some((o) => isNaN(o) || o > 255)) {
      throw new Error("invalid ip address")
    }
    return new IpAddress(octets, address)
  }
  static fromOctets(octets: number[]) {
    if (octets.length == 4) {
      if (octets.some((o) => o > 255)) {
        throw new Error("invalid ip address")
      }
      return new IpAddress(octets, octets.join("."))
    } else if (octets.length == 16) {
      if (octets.some((o) => o > 255)) {
        throw new Error("invalid ip address")
      }
      let pre = octets.slice(0, 8)
      while (pre[pre.length - 1] == 0) {
        pre.pop()
      }
      let post = octets.slice(8)
      while (post[0] == 0) {
        post.unshift()
      }
      if (pre.length + post.length == 16) {
        return new IpAddress(octets, octets.join(":"))
      } else {
        return new IpAddress(octets, pre.join(":") + "::" + post.join(":"))
      }
    } else {
      throw new Error("invalid ip address")
    }
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
  add(n: number): IpAddress {
    let octets = [...this.octets]
    n = Math.floor(n)
    for (let i = octets.length - 1; i >= 0; i--) {
      octets[i] += n
      if (octets[i] > 255) {
        n = octets[i] >> 8
        octets[i] &= 255
      } else {
        break
      }
    }
    if (octets[0] > 255) {
      throw new Error("overflow incrementing ip")
    }
    return IpAddress.fromOctets(octets)
  }
  sub(n: number): IpAddress {
    let octets = [...this.octets]
    n = Math.floor(n)
    for (let i = octets.length - 1; i >= 0; i--) {
      octets[i] -= n
      if (octets[i] < 0) {
        n = Math.ceil(Math.abs(octets[i]) / 256)
        octets[i] = ((octets[i] % 256) + 256) % 256
      } else {
        break
      }
    }
    if (octets[0] < 0) {
      throw new Error("underflow decrementing ip")
    }
    return IpAddress.fromOctets(octets)
  }
  cmp(other: string | IpAddress): -1 | 0 | 1 {
    if (typeof other === "string") other = IpAddress.parse(other)
    const len = Math.max(this.octets.length, other.octets.length)
    for (let i = 0; i < len; i++) {
      const left = this.octets[i] || 0
      const right = other.octets[i] || 0
      if (left > right) {
        return 1
      } else if (left < right) {
        return -1
      }
    }
    return 0
  }
}

export class IpNet extends IpAddress {
  private constructor(
    octets: number[],
    readonly prefix: number,
    address: string,
    readonly ipnet: string,
  ) {
    super(octets, address)
  }
  static fromIpPrefix(ip: IpAddress, prefix: number): IpNet {
    if (prefix > ip.octets.length * 8) {
      throw new Error("invalid prefix")
    }
    return new IpNet(ip.octets, prefix, ip.address, `${ip.address}/${prefix}`)
  }
  static parse(ipnet: string): IpNet {
    const [address, prefixStr] = ipnet.split("/", 2)
    const ip = IpAddress.parse(address)
    const prefix = Number(prefixStr)
    return IpNet.fromIpPrefix(ip, prefix)
  }
  contains(address: string | IpAddress): boolean {
    if (typeof address === "string") address = IpAddress.parse(address)
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
    const mask = 255 ^ (255 >> prefix)
    return (this.octets[idx] & mask) === (address.octets[idx] & mask)
  }
  zero(): IpAddress {
    let octets: number[] = []
    let prefix = this.prefix
    for (let idx = 0; idx < this.octets.length; idx++) {
      if (prefix >= 8) {
        octets[idx] = this.octets[idx]
        prefix -= 8
      } else {
        const mask = 255 ^ (255 >> prefix)
        octets[idx] = this.octets[idx] & mask
        prefix = 0
      }
    }

    return IpAddress.fromOctets(octets)
  }
  broadcast(): IpAddress {
    let octets: number[] = []
    let prefix = this.prefix
    for (let idx = 0; idx < this.octets.length; idx++) {
      if (prefix >= 8) {
        octets[idx] = this.octets[idx]
        prefix -= 8
      } else {
        const mask = 255 >> prefix
        octets[idx] = this.octets[idx] | mask
        prefix = 0
      }
    }

    return IpAddress.fromOctets(octets)
  }
}

export const PRIVATE_IPV4_RANGES = [
  IpNet.parse("127.0.0.0/8"),
  IpNet.parse("10.0.0.0/8"),
  IpNet.parse("172.16.0.0/12"),
  IpNet.parse("192.168.0.0/16"),
]

export const IPV4_LOOPBACK = IpNet.parse("127.0.0.0/8")
export const IPV6_LOOPBACK = IpNet.parse("::1/128")
export const IPV6_LINK_LOCAL = IpNet.parse("fe80::/10")

export const CGNAT = IpNet.parse("100.64.0.0/10")
