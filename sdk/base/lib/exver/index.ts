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

// !(#o:=2) && !(#o:=3)
// (<2 || >2 || !#o) && (<3 || >3 || !#o)
// (<3 && (<2 || >2 || !#o)) || (>3 && (<2 || >2 || !#o)) || (!#o && (<2 || >2 || !#o))
// <3 && <2 || <3 && >2 || <3 && !#o || >3 && <2 || >3 && >2 || >3 && !#o || !#o && <2 || !#o && >2 || !#o && !#o


type Flavor = {
  type: "Flavor",
  flavor: string | null,
}

type FlavorNot = {
  type: "FlavorNot",
  flavor: string | null,
}

class VersionRangeEnd {
  constructor(
    public inclusive: boolean,
    public upstream: Version,
    public downstream: Version,
  ) {}

  static compare(self: VersionRangeEnd | 'Inf', other: VersionRangeEnd | 'Inf', side: 'lo' | 'hi') {
    if (self == 'Inf' && other == 'Inf') {
      return 'equal';
    }
    if (self == 'Inf') {
      return side == 'lo' ? 'less' : 'greater';
    }
    if (other == 'Inf') {
      return side == 'lo' ? 'greater' : 'less';
    }

    let cmp = self.upstream.compare(other.upstream);
    if (cmp != 'equal') {
      return cmp
    }
    cmp = self.downstream.compare(other.downstream);
    if (cmp != 'equal') {
      return cmp;
    }

    if (self.inclusive && other.inclusive) {
      return 'equal';
    }
    if (self.inclusive) {
      return side == 'lo' ? 'less' : 'greater';
    }
    if (other.inclusive) {
      return side == 'lo' ? 'greater' : 'less';
    }

    return 'equal';
  }

  static lower(self: VersionRangeEnd | 'Inf', other: VersionRangeEnd | 'Inf') {
    if (VersionRangeEnd.compare(self, other, 'lo') == 'less') {
      return self;
    } else {
      return other;
    }
  }

  static higher(self: VersionRangeEnd | 'Inf', other: VersionRangeEnd | 'Inf') {
    if (VersionRangeEnd.compare(self, other, 'hi') == 'greater') {
      return self;
    } else {
      return other;
    }
  }
}

class ContiguousVersionRange {
  constructor(
    public flavor: Flavor | FlavorNot[] | "Any",
    public lo: VersionRangeEnd | "Inf",
    public hi: VersionRangeEnd | "Inf",
  ) {}

  and(other: ContiguousVersionRange): ContiguousVersionRange | null {
    let flavor: Flavor | FlavorNot[] | "Any";
    if (this.flavor != 'Any' && other.flavor != 'Any') {
      if (Array.isArray(this.flavor)) {
        if (Array.isArray(other.flavor)) {
          flavor = this.flavor.concat(other.flavor);
        } else if (this.flavor.find(x => x.flavor == other.flavor) === undefined) {
          flavor = other.flavor;
        } else {
          return null;
        }
      } else {
        if (Array.isArray(other.flavor)) {
          if (other.flavor.find(x => x.flavor == this.flavor) === undefined) {
            flavor = other.flavor;
          } else {
            return null;
          }
        } else {
          if (this.flavor == other.flavor) {
            flavor = this.flavor;
          } else {
            return null;
          }
        }
      }
    } else {
      flavor = 'Any';
    }

    if (other.hi < this.lo || this.hi < other.lo) {
      return null;
    }
    return new ContiguousVersionRange(
      flavor,
      VersionRangeEnd.lower(this.lo, other.lo),
      VersionRangeEnd.higher(this.hi, other.hi),
    )
  }
}

type VersionRangeList = ContiguousVersionRange[]

export class VersionRange {
  private constructor(public atom: Anchor | And | Or | Not | Flavor | FlavorNot | P.Any | P.None) {}

  toStringParens(parent: "And" | "Or" | "Not") {
    let needs = true;
    switch (this.atom.type) {
      case "And":
      case "Or":
        needs = parent != this.atom.type;
        break
      case "Anchor":
      case "Any":
      case "None":
        needs = parent == "Not"
        break
      case "Not":
      case "Flavor":
      case "FlavorNot":
        needs = false;
        break
    }

    if (needs) {
      return "(" + this.toString() + ")";
    } else {
      return this.toString();
    }
  }

  toString(): string {
    switch (this.atom.type) {
      case "Anchor":
        return `${this.atom.operator}${this.atom.version}`
      case "And":
        return `${this.atom.left.toStringParens(this.atom.type)} && ${this.atom.right.toStringParens(this.atom.type)}`
      case "Or":
        return `${this.atom.left.toStringParens(this.atom.type)} || ${this.atom.right.toStringParens(this.atom.type)}`
      case "Not":
        return `!${this.atom.value.toStringParens(this.atom.type)}`
      case "Flavor":
        return `#${this.atom.flavor}`
      case "FlavorNot":
        return `!#${this.atom.flavor}`
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

  and(right: VersionRange): VersionRange {
    if (this.atom.type == "None" || right.atom.type == "None") {
      return VersionRange.none()
    }
    if (this.atom.type == "Any") {
      return right
    }
    if (right.atom.type == "Any") {
      return this
    }
    if (right.atom.type == "Or") {
      return this.and(right.atom.left).or(this.and(right.atom.right))
    }
    if (this.atom.type == "Or") {
      return this.atom.left.and(right).or(this.atom.right.and(right))
    }
    return new VersionRange({ type: "And", left: this, right })
  }

  or(right: VersionRange): VersionRange {
    if (this.atom.type == "Any" || right.atom.type == "Any") {
      return VersionRange.any()
    }
    if (this.atom.type == "None") {
      return right
    }
    if (right.atom.type == "None") {
      return this
    }
    return new VersionRange({ type: "Or", left: this, right })
  }

  not(): VersionRange {
    switch (this.atom.type) {
      case "Anchor":
        const fn = VersionRange.flavorNot(this.atom.version.flavor);
        switch (this.atom.operator) {
          case "=":
            return VersionRange.anchor("!=", this.atom.version)
          case "!=":
            return VersionRange.anchor("=", this.atom.version)
          case ">":
            return VersionRange.anchor("<=", this.atom.version).or(fn)
          case "<":
            return VersionRange.anchor(">=", this.atom.version).or(fn)
          case ">=":
            return VersionRange.anchor("<", this.atom.version).or(fn)
          case "<=":
            return VersionRange.anchor(">", this.atom.version).or(fn)
          case "^":
          case "~":
            return new VersionRange({ type: "Not", value: this })
        }
      case "And":
        return this.atom.left.not().or(this.atom.right.not())
      case "Or":
        return this.atom.left.not().and(this.atom.right.not())
      case "Not":
        return this.atom.value
      case "Any":
        return VersionRange.none()
      case "None":
        return VersionRange.any()
      case "Flavor":
        return VersionRange.flavorNot(this.atom.flavor)
      case "FlavorNot":
        return VersionRange.flavor(this.atom.flavor)
    }
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

  static flavor(flavor: string | null) {
    return new VersionRange({ type: "Flavor", flavor })
  }

  static flavorNot(flavor: string | null) {
    return new VersionRange({ type: "FlavorNot", flavor })
  }

  satisfiedBy(version: Version | ExtendedVersion) {
    return version.satisfies(this)
  }

  normalize(): VersionRange {
    switch (this.atom.type) {
      case "Anchor":
        switch (this.atom.operator) {
          case "!=":
            return VersionRange.anchor("<", this.atom.version).or(VersionRange.anchor(">", this.atom.version)).or(VersionRange.flavorNot(this.atom.version.flavor))
          case "^":
            return VersionRange.anchor(">=", this.atom.version).and(VersionRange.anchor("<", this.atom.version.incrementMajor()))
          case "~":
            return VersionRange.anchor(">=", this.atom.version).and(VersionRange.anchor("<", this.atom.version.incrementMinor()))
          default:
            return this
        }
      case "And":
        return this.atom.left.normalize().and(this.atom.right.normalize())
      case "Or":
        return this.atom.left.normalize().or(this.atom.right.normalize())
      case "Not":
        return this.atom.value.normalize().not()
      case "Flavor":
        return VersionRange.flavor(this.atom.flavor)
      case "FlavorNot":
        return VersionRange.flavorNot(this.atom.flavor)
      case "Any":
        return this
      case "None":
        return VersionRange.none()
        
    }
  }

  private flattenUnsorted(): VersionRangeList {
    let norm = this.normalize();
    let left;
    let right;
    let here: ExtendedVersion;
    let flavor: Flavor;
    switch(norm.atom.type) {
      case "Anchor":
        here = norm.atom.version;
        flavor = { type: "Flavor", flavor: here.flavor };
        switch (norm.atom.operator) {
          case "=":
            return [new ContiguousVersionRange(flavor, here.inclusive(), here.inclusive())]
          case ">":
            return [new ContiguousVersionRange(flavor, here.exclusive(), "Inf")]
          case "<":
            return [new ContiguousVersionRange(flavor, "Inf", here.exclusive())]
          case ">=":
            return [new ContiguousVersionRange(flavor, here.inclusive(), "Inf")]
          case "<=":
            return [new ContiguousVersionRange(flavor, "Inf", here.inclusive())]
          case "!=":
          case "^":
          case "~":
            throw Error('not normalized')
        }
      case "Not":
        throw Error('not normalized')
      case "And":
        left = norm.atom.left.flattenUnsorted();
        right = norm.atom.right.flattenUnsorted();
        if (left.length == 0 || right.length == 0) {
          return []
        }
        if (left.length != 1 || right.length != 1) {
          throw Error('not normalized')
        }
        return [left[0].and(right[0])]
      case "Or":
        left = norm.atom.left.flattenUnsorted();
        right = norm.atom.right.flattenUnsorted();
        return left.concat(right)
      case "Flavor":
      case "FlavorNot":
        return [new ContiguousVersionRange(norm.atom, "Inf", "Inf")]
      case "Any":
        return [new ContiguousVersionRange("Any", "Inf", "Inf")]
      case "None":
        return []
    }
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
    const parsed = P.parse(extendedVersion, { startRule: "EmVer" })
    return new ExtendedVersion(
      parsed.flavor,
      new Version(parsed.upstream.number, parsed.upstream.prerelease),
      new Version(parsed.downstream.number, parsed.downstream.prerelease),
    )
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

  inclusive(): VersionRangeEnd {
    return new VersionRangeEnd(true, this.upstream, this.downstream)
  }

  exclusive(): VersionRangeEnd {
    return new VersionRangeEnd(false, this.upstream, this.downstream)
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
