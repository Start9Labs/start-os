import * as P from "./exver"

// prettier-ignore
export type ValidateVersion<T extends String> = 
T extends `-${infer A}` ? never  :
T extends `${infer A}-${string}` ? ValidateVersion<A> :
  T extends `${bigint}` ? unknown :
  T extends `${bigint}.${infer A}` ? ValidateVersion<A> :
  never

// prettier-ignore
export type ValidateExVer<T extends string> = 
  T extends `#${string}:${infer A}:${infer B}` ? ValidateVersion<A> & ValidateVersion<B> :
  T extends `${infer A}:${infer B}` ? ValidateVersion<A> & ValidateVersion<B> :  
  never

// prettier-ignore
export type ValidateExVers<T> =
  T extends [] ? unknown[] :
  T extends [infer A, ...infer B] ? ValidateExVer<A & string> & ValidateExVers<B> :
  never[]

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
  private constructor(public atom: Anchor | And | Or | Not | P.Any | P.None) {}

  toString(): string {
    switch (this.atom.type) {
      case "Anchor":
        return `${this.atom.operator}${this.atom.version}`
      case "And":
        return `(${this.atom.left.toString()}) && (${this.atom.right.toString()})`
      case "Or":
        return `(${this.atom.left.toString()}) || (${this.atom.right.toString()})`
      case "Not":
        return `!(${this.atom.value.toString()})`
      case "Any":
        return "*"
      case "None":
        return "!"
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
      switch (next[1]?.[0]) {
        case "||":
          result = new VersionRange({
            type: "Or",
            left: result,
            right: VersionRange.parseAtom(next[2]),
          })
          break
        case "&&":
        default:
          result = new VersionRange({
            type: "And",
            left: result,
            right: VersionRange.parseAtom(next[2]),
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

  static parseEmver(range: string): VersionRange {
    return VersionRange.parseRange(
      P.parse(range, { startRule: "EmverVersionRange" }),
    )
  }

  and(right: VersionRange) {
    return new VersionRange({ type: "And", left: this, right })
  }

  or(right: VersionRange) {
    return new VersionRange({ type: "Or", left: this, right })
  }

  not() {
    return new VersionRange({ type: "Not", value: this })
  }

  static anchor(operator: P.CmpOp, version: ExtendedVersion) {
    return new VersionRange({ type: "Anchor", operator, version })
  }

  static any() {
    return new VersionRange({ type: "Any" })
  }

  static none() {
    return new VersionRange({ type: "None" })
  }

  satisfiedBy(version: Version | ExtendedVersion) {
    return version.satisfies(this)
  }
}

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

  static parse(version: string): Version {
    const parsed = P.parse(version, { startRule: "Version" })
    return new Version(parsed.number, parsed.prerelease)
  }

  satisfies(versionRange: VersionRange): boolean {
    return new ExtendedVersion(null, this, new Version([0], [])).satisfies(
      versionRange,
    )
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

  greaterThanOrEqual(other: ExtendedVersion): boolean {
    return ["greater", "equal"].includes(this.compare(other) as string)
  }

  equals(other: ExtendedVersion): boolean {
    return this.compare(other) === "equal"
  }

  lessThan(other: ExtendedVersion): boolean {
    return this.compare(other) === "less"
  }

  lessThanOrEqual(other: ExtendedVersion): boolean {
    return ["less", "equal"].includes(this.compare(other) as string)
  }

  static parse(extendedVersion: string): ExtendedVersion {
    const parsed = P.parse(extendedVersion, { startRule: "ExtendedVersion" })
    return new ExtendedVersion(
      parsed.flavor,
      new Version(parsed.upstream.number, parsed.upstream.prerelease),
      new Version(parsed.downstream.number, parsed.downstream.prerelease),
    )
  }

  static parseEmver(extendedVersion: string): ExtendedVersion {
    try {
      const parsed = P.parse(extendedVersion, { startRule: "Emver" })
      return new ExtendedVersion(
        parsed.flavor,
        new Version(parsed.upstream.number, parsed.upstream.prerelease),
        new Version(parsed.downstream.number, parsed.downstream.prerelease),
      )
    } catch (e) {
      if (e instanceof Error) {
        e.message += ` (${extendedVersion})`
      }
      throw e
    }
  }

  /**
   * Returns an ExtendedVersion with the Upstream major version version incremented by 1
   * and sets subsequent digits to zero.
   * If no non-zero upstream digit can be found the last upstream digit will be incremented.
   */
  incrementMajor(): ExtendedVersion {
    const majorIdx = this.upstream.number.findIndex((num: number) => num !== 0)

    const majorNumber = this.upstream.number.map((num, idx): number => {
      if (idx > majorIdx) {
        return 0
      } else if (idx === majorIdx) {
        return num + 1
      }
      return num
    })

    const incrementedUpstream = new Version(majorNumber, [])
    const updatedDownstream = new Version([0], [])

    return new ExtendedVersion(
      this.flavor,
      incrementedUpstream,
      updatedDownstream,
    )
  }

  /**
   * Returns an ExtendedVersion with the Upstream minor version version incremented by 1
   * also sets subsequent digits to zero.
   * If no non-zero upstream digit can be found the last digit will be incremented.
   */
  incrementMinor(): ExtendedVersion {
    const majorIdx = this.upstream.number.findIndex((num: number) => num !== 0)
    let minorIdx = majorIdx === -1 ? majorIdx : majorIdx + 1

    const majorNumber = this.upstream.number.map((num, idx): number => {
      if (idx > minorIdx) {
        return 0
      } else if (idx === minorIdx) {
        return num + 1
      }
      return num
    })

    const incrementedUpstream = new Version(majorNumber, [])
    const updatedDownstream = new Version([0], [])

    return new ExtendedVersion(
      this.flavor,
      incrementedUpstream,
      updatedDownstream,
    )
  }

  /**
   * Returns a boolean indicating whether a given version satisfies the VersionRange
   * !( >= 1:1 <= 2:2) || <=#bitcoin:1.2.0-alpha:0
   */
  satisfies(versionRange: VersionRange): boolean {
    switch (versionRange.atom.type) {
      case "Anchor":
        const otherVersion = versionRange.atom.version
        switch (versionRange.atom.operator) {
          case "=":
            return this.equals(otherVersion)
          case ">":
            return this.greaterThan(otherVersion)
          case "<":
            return this.lessThan(otherVersion)
          case ">=":
            return this.greaterThanOrEqual(otherVersion)
          case "<=":
            return this.lessThanOrEqual(otherVersion)
          case "!=":
            return !this.equals(otherVersion)
          case "^":
            const nextMajor = versionRange.atom.version.incrementMajor()
            if (
              this.greaterThanOrEqual(otherVersion) &&
              this.lessThan(nextMajor)
            ) {
              return true
            } else {
              return false
            }
          case "~":
            const nextMinor = versionRange.atom.version.incrementMinor()
            if (
              this.greaterThanOrEqual(otherVersion) &&
              this.lessThan(nextMinor)
            ) {
              return true
            } else {
              return false
            }
        }
      case "And":
        return (
          this.satisfies(versionRange.atom.left) &&
          this.satisfies(versionRange.atom.right)
        )
      case "Or":
        return (
          this.satisfies(versionRange.atom.left) ||
          this.satisfies(versionRange.atom.right)
        )
      case "Not":
        return !this.satisfies(versionRange.atom.value)
      case "Any":
        return true
      case "None":
        return false
    }
  }
}

export const testTypeExVer = <T extends string>(t: T & ValidateExVer<T>) => t

export const testTypeVersion = <T extends string>(t: T & ValidateVersion<T>) =>
  t
function tests() {
  testTypeVersion("1.2.3")
  testTypeVersion("1")
  testTypeVersion("12.34.56")
  testTypeVersion("1.2-3")
  testTypeVersion("1-3")
  testTypeVersion("1-alpha")
  // @ts-expect-error
  testTypeVersion("-3")
  // @ts-expect-error
  testTypeVersion("1.2.3:1")
  // @ts-expect-error
  testTypeVersion("#cat:1:1")

  testTypeExVer("1.2.3:1.2.3")
  testTypeExVer("1.2.3.4.5.6.7.8.9.0:1")
  testTypeExVer("100:1")
  testTypeExVer("#cat:1:1")
  testTypeExVer("1.2.3.4.5.6.7.8.9.11.22.33:1")
  testTypeExVer("1-0:1")
  testTypeExVer("1-0:1")
  // @ts-expect-error
  testTypeExVer("1.2-3")
  // @ts-expect-error
  testTypeExVer("1-3")
  // @ts-expect-error
  testTypeExVer("1.2.3.4.5.6.7.8.9.0.10:1" as string)
  // @ts-expect-error
  testTypeExVer("1.-2:1")
  // @ts-expect-error
  testTypeExVer("1..2.3:3")
}
