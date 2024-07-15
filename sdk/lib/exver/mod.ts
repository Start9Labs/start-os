const exverParser = require("./exver.js")

export interface ValidExtendedVersion {
  flavor: string | null
  upstream: Version
  downstream: Version
}

export interface Version {
  number: number[]
  prerelease: (string | number)[]
}

// !( >=1:1 && <= 2:2)
type Operator = ">" | "<" | ">=" | "<=" | "!=" | "^" | "~" | "="

type Anchor = {
  type: "Anchor"
  operator: Operator
  version: ExtendedVersion
}

type And = {
  type: "And"
  left: VersionRange
  right: VersionRange
}

type Or = {
  type: "Or"
  left: VersionRange
  right: VersionRange
}

type Not = {
  type: "Not"
  value: VersionRange
}

type Any = {
  type: "Any"
}

type None = {
  type: "None"
}

class VersionRange {
  atom: Anchor | And | Or | Not | Any | None
  constructor(atomInput: Anchor | And | Or | Not | Any | None) {
    this.atom = atomInput
  }

  /**
   * Returns a boolean indicating whether a given version satisfies the VersionRange
   * !( >= 1:1 <= 2:2) || <=#bitcoin:1.2.0-alpha:0
   */
  public satisfiedBy(version: ExtendedVersion): boolean {
    switch (this.atom.type) {
      case "Anchor":
        const otherVersion = this.atom.version
        switch (this.atom.operator) {
          case "=":
            return version.equals(otherVersion)
          case ">":
            return version.greaterThan(otherVersion)
          case "<":
            return version.lessThan(otherVersion)
          case ">=":
            return (
              version.greaterThan(otherVersion) || version.equals(otherVersion)
            )
          case "<=":
            return (
              version.lessThan(otherVersion) || version.equals(otherVersion)
            )
          case "!=":
            return !version.equals(otherVersion)
          case "^":
          case "~":
        }
    }
  }
}

class VersionRangeConstructor {
  static parseRange(range: VersionRange): VersionRange {
    switch (range.atom.type) {
      case "Anchor":
        const anchor: Anchor = {
          type: "Anchor",
          operator: range.atom.operator,
          version: range.atom.version,
        }
        return new VersionRange(anchor)
      case "And":
        const and: And = {
          type: "And",
          left: range.atom.left,
          right: range.atom.right,
        }
        return this.parseRange(new VersionRange(and))
      case "Or":
        const or: Or = {
          type: "Or",
          left: range.atom.left,
          right: range.atom.right,
        }
        return this.parseRange(new VersionRange(or))
      case "Not":
        const not: Not = {
          type: "Not",
          value: range.atom.value,
        }
        return this.parseRange(new VersionRange(not))
      case "Any":
        const any: Any = {
          type: "Any",
        }
        return new VersionRange(any)
      case "None":
        const none: None = {
          type: "None",
        }
        return new VersionRange(none)
    }
  }
}

//   public satisfiedBy(thisVersion: ExtendedVersion): boolean {
//     if (this.negator) {
//       if (this.anchor.flavor !== thisVersion.flavor) return true
//       switch (this.operator) {
//         case ">=": {
//           return !(thisVersion.greaterThan(this.anchor) || thisVersion.equals(this.anchor))
//         }
//         case "<=": {
//           return !(thisVersion.lessThan(this.anchor) || thisVersion.equals(this.anchor))
//         }
//         case ">": {
//           return !thisVersion.greaterThan(this.anchor)
//         }
//         case "<": {
//           return !thisVersion.lessThan(this.anchor)
//         }
//         case "=": {
//           return !thisVersion.equals(this.anchor)
//         }
//       }
//     } else {
//       if (this.anchor.flavor !== thisVersion.flavor) return false
//       switch (this.operator) {
//         case ">=": {
//           return thisVersion.greaterThan(this.anchor) || thisVersion.equals(this.anchor)
//         }
//         case "<=": {
//           return thisVersion.lessThan(this.anchor) || thisVersion.equals(this.anchor)
//         }
//         case ">": {
//           return thisVersion.greaterThan(this.anchor)
//         }
//         case "<": {
//           return thisVersion.lessThan(this.anchor)
//         }
//         case "=": {
//           return thisVersion.equals(this.anchor)
//         }
//       }
//     }
//     throw new Error("Error parsing range" + this.operator + this.anchor)
//   }
// }

// #flavor:0.1.2-beta.1:0
export class ExtendedVersion implements ValidExtendedVersion {
  flavor: string | null
  upstream: Version
  downstream: Version

  constructor(exverString: string) {
    const exver = exverParser.parse(exverString)
    this.flavor = exver.flavor
    this.upstream = exver.upstream
    this.downstream = exver.downstream
  }

  toString(): string {
    let exver_str = ""

    if (this.flavor) {
      exver_str += "#"
      exver_str += this.flavor
      exver_str += ":"
    }

    exver_str = appendVersion(this.upstream, exver_str)
    exver_str += ":"
    exver_str = appendVersion(this.downstream, exver_str)

    return exver_str
  }

  public greaterThan(other: ExtendedVersion): boolean {
    return greaterThan(this, other)
  }

  public equals(other: ExtendedVersion): boolean {
    return (
      equals(this.upstream, other.upstream) &&
      equals(this.downstream, other.downstream)
    )
  }

  public lessThan(other: ExtendedVersion): boolean {
    return !this.greaterThan(other)
  }

  /**
   * Compare with `other` ExtendedVersion for sort
   */
  public compare(other: ExtendedVersion) {
    if (this.flavor !== other.flavor) {
      return null
    } else if (this.equals(other)) {
      return 0 as const
    } else if (this.greaterThan(other)) {
      return 1 as const
    } else {
      return -1 as const
    }
  }
}

function greaterThan(
  thisVersion: ExtendedVersion,
  otherVersion: ExtendedVersion,
): boolean {
  const upstreamGtResult = versionGt(
    thisVersion.upstream,
    otherVersion.upstream,
  )
  if (typeof upstreamGtResult === "boolean") {
    return upstreamGtResult
  }
  const downstreamGtResult = versionGt(
    thisVersion.downstream,
    otherVersion.downstream,
  )
  if (typeof downstreamGtResult === "boolean") {
    return downstreamGtResult
  }
  return false
}

function versionGt(
  thisVersion: Version,
  otherVersion: Version,
): boolean | undefined {
  for (const i in thisVersion.number) {
    if (otherVersion.number[i] == null) {
      return true
    }
    if (thisVersion.number[i] > otherVersion.number[i]) {
      return true
    }
    if (thisVersion.number[i] < otherVersion.number[i]) {
      return false
    }
  }
  for (const i in thisVersion.prerelease) {
    if (thisVersion.prerelease[i] > otherVersion.prerelease[i]) {
      return true
    } else if (thisVersion.prerelease[i] < otherVersion.prerelease[i]) {
      return false
    }
  }
}

function equals(thisVersion: Version, otherVersion: Version): boolean {
  for (const i in thisVersion.number) {
    if (thisVersion.number[i] !== otherVersion.number[i]) {
      return false
    }
  }
  for (const i in thisVersion.prerelease) {
    if (thisVersion.prerelease[i] !== otherVersion.prerelease[i]) {
      return false
    }
  }
  return true
}

function appendVersion(version: Version, str: string): string {
  version.number.forEach((n, i) => {
    str += n
    if (i < version.number.length - 1) {
      str += "."
    }
  })

  if (version.prerelease.length > 0) {
    str += "-"
    version.prerelease.forEach((n, i) => {
      str += n
      if (i < version.prerelease.length - 1) {
        str += "."
      }
    })
  }

  return str
}
