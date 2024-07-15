import * as P from "./exver"

type Anchor = {
  type: "Anchor"
  operator: P.CmpOp
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

export class VersionRange {
  private constructor(private atom: Anchor | And | Or | Not | P.Any | P.None) {}

  /**
   * Returns a boolean indicating whether a given version satisfies the VersionRange
   * !( >= 1:1 <= 2:2) || <=#bitcoin:1.2.0-alpha:0
   */
  satisfiedBy(version: ExtendedVersion): boolean {
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

  private static parseAtom(atom: P.VersionRangeAtom): VersionRange {
    switch (atom.type) {
      case "Not":
        return new VersionRange({
          type: "Not",
          value: VersionRange.parseAtom(atom.value),
        })
      case "Parens":
        return VersionRange.parseRange(atom.expr)
      case "Anchor":
        return new VersionRange({
          type: "Anchor",
          operator: atom.operator || "^",
          version: new ExtendedVersion(
            atom.version.flavor,
            new Version(
              atom.version.upstream.number,
              atom.version.upstream.prerelease,
            ),
            new Version(
              atom.version.downstream.number,
              atom.version.downstream.prerelease,
            ),
          ),
        })
      default:
        return new VersionRange(atom)
    }
  }

  private static parseRange(range: P.VersionRange): VersionRange {
    let result = VersionRange.parseAtom(range[0])
    for (const next of range[1]) {
      switch (next[0]) {
        case "||":
          result = new VersionRange({
            type: "Or",
            left: result,
            right: VersionRange.parseAtom(next[1]),
          })
          break
        case "&&":
        case null:
          result = new VersionRange({
            type: "And",
            left: result,
            right: VersionRange.parseAtom(next[1]),
          })
          break
      }
    }
    return result
  }

  static parse(range: string): VersionRange {
    return VersionRange.parseRange(
      P.parse(range, { startRule: "VersionRange" }),
    )
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

export class Version {
  constructor(
    public number: number[],
    public prerelease: (string | number)[],
  ) {}

  toString(): string {
    return `${this.number.join(".")}${this.prerelease.length > 0 ? `-${this.prerelease.join(".")}` : ""}`
  }

  compare(other: Version): "greater" | "equal" | "less" {
    const numLen = Math.max(this.number.length, other.number.length)
    for (let i = 0; i < numLen; i++) {
      if ((this.number[i] || 0) > (other.number[i] || 0)) {
        return "greater"
      } else if ((this.number[i] || 0) < (other.number[i] || 0)) {
        return "less"
      }
    }

    if (this.prerelease.length === 0 && other.prerelease.length !== 0) {
      return "greater"
    } else if (this.prerelease.length !== 0 && other.prerelease.length === 0) {
      return "less"
    }

    const prereleaseLen = Math.max(this.number.length, other.number.length)
    for (let i = 0; i < prereleaseLen; i++) {
      if (typeof this.prerelease[i] === typeof other.prerelease[i]) {
        if (this.prerelease[i] > other.prerelease[i]) {
          return "greater"
        } else if (this.prerelease[i] < other.prerelease[i]) {
          return "less"
        }
      } else {
        switch (`${typeof this.prerelease[1]}:${typeof other.prerelease[i]}`) {
          case "number:string":
            return "less"
          case "string:number":
            return "greater"
          case "number:undefined":
          case "string:undefined":
            return "greater"
          case "undefined:number":
          case "undefined:string":
            return "less"
        }
      }
    }

    return "equal"
  }

  parse(version: string): Version {
    const parsed = P.parse(version, { startRule: "Version" })
    return new Version(parsed.number, parsed.prerelease)
  }
}

// #flavor:0.1.2-beta.1:0
export class ExtendedVersion {
  constructor(
    public flavor: string | null,
    public upstream: Version,
    public downstream: Version,
  ) {}

  toString(): string {
    return `${this.flavor ? `#${this.flavor}:` : ""}${this.upstream.toString()}:${this.downstream.toString()}`
  }

  compare(other: ExtendedVersion): "greater" | "equal" | "less" | null {
    if (this.flavor !== other.flavor) {
      return null
    }
    const upstreamCmp = this.upstream.compare(other.upstream)
    if (upstreamCmp !== "equal") {
      return upstreamCmp
    }
    return this.downstream.compare(other.downstream)
  }

  compareLexicographic(other: ExtendedVersion): "greater" | "equal" | "less" {
    if ((this.flavor || "") > (other.flavor || "")) {
      return "greater"
    } else if ((this.flavor || "") > (other.flavor || "")) {
      return "less"
    } else {
      return this.compare(other)!
    }
  }

  compareForSort(other: ExtendedVersion): 1 | 0 | -1 {
    switch (this.compareLexicographic(other)) {
      case "greater":
        return 1
      case "equal":
        return 0
      case "less":
        return -1
    }
  }

  greaterThan(other: ExtendedVersion): boolean {
    return this.compare(other) === "greater"
  }

  equals(other: ExtendedVersion): boolean {
    return this.compare(other) === "equal"
  }

  lessThan(other: ExtendedVersion): boolean {
    return this.compare(other) === "less"
  }

  parse(extendedVersion: string): ExtendedVersion {
    const parsed = P.parse(extendedVersion, { startRule: "ExtendedVersion" })
    return new ExtendedVersion(
      parsed.flavor,
      new Version(parsed.upstream.number, parsed.upstream.prerelease),
      new Version(parsed.downstream.number, parsed.downstream.prerelease),
    )
  }
}
