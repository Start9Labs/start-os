import { DeepMap } from "deep-equality-data-structures";
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
  operator: P.CmpOp,
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

type Flavor = {
  type: "Flavor",
  flavor: string | null,
}

type FlavorNot = {
  type: "FlavorNot",
  flavors: Set<string | null>,
}

type FlavorAtom = Flavor | FlavorNot;

type VersionRangePoint = {
  upstream: Version,
  downstream: Version,
  side: -1 | 1;
}

function compareVersionRangePoints(a: VersionRangePoint, b: VersionRangePoint): -1 | 0 | 1 {
  let up = a.upstream.compareForSort(b.upstream);
  if (up != 0) {
    return up;
  }
  let down = a.upstream.compareForSort(b.upstream);
  if (down != 0) {
    return down;
  }
  if (a.side < b.side) {
    return -1;
  } else if (a.side > b.side) {
    return 1;
  } else {
    return 0;
  }
}

function adjacentVersionRangePoints(a: VersionRangePoint, b: VersionRangePoint): boolean {
  let up = a.upstream.compareForSort(b.upstream);
  if (up != 0) {
    return false;
  }
  let down = a.upstream.compareForSort(b.upstream);
  if (down != 0) {
    return false;
  }
  return a.side == -1 && b.side == 1;
}

function flavorAnd(a: FlavorAtom, b: FlavorAtom): FlavorAtom | null {
  if (a.type == 'Flavor') {
    if (b.type == 'Flavor') {
      if (a.flavor == b.flavor) {
        return a;
      } else {
        return null;
      }
    } else {
      if (b.flavors.has(a.flavor)) {
        return null;
      } else {
        return a;
      }
    }
  } else {
    if (b.type == 'Flavor') {
      if (a.flavors.has(b.flavor)) {
        return null;
      } else {
        return b;
      }
    } else {
      return { type: 'FlavorNot', flavors: a.flavors.union(b.flavors) };
    }
  }
}

type VersionRangeTables = DeepMap<FlavorAtom, VersionRangeTable> | boolean;

class VersionRangeTable {
  private constructor(protected points: Array<VersionRangePoint>, protected values: boolean[]) {}

  static zip(a: VersionRangeTable, b: VersionRangeTable, func: (a: boolean, b: boolean) => boolean): VersionRangeTable {
    let c = new VersionRangeTable([], []);
    let i = 0;
    let j = 0;
    while (true) {
      let next = func(a.values[i], b.values[j]);
      if (c.values.length > 0 && c.values[c.values.length - 1] == next) {
        // collapse automatically
        c.points.pop();
      } else {
        c.values.push(next);
      }

      // which point do we step over?
      if (i == a.points.length) {
        if (j == b.points.length) {
          // just added the last segment, no point to jump over
          return c;
        } else {
          // i has reach the end, step over j
          c.points.push(b.points[j]);
          j += 1;
        }
      } else {
        if (j == b.points.length) {
          // j has reached the end, step over i
          c.points.push(a.points[i]);
          i += 1;
        } else {
          // depends on which of the next two points is lower
          switch (compareVersionRangePoints(a.points[i], b.points[j])) {
            case -1:
              // i is the lower point
              c.points.push(a.points[i]);
              i += 1;
              break;
            case 1:
              // j is the lower point
              c.points.push(b.points[j]);
              j += 1;
              break;
            default:
              // step over both
              c.points.push(a.points[i]);
              i += 1;
              j += 1;
              break;
          }
        }
      }
    }
  }

  static full(flavor: string | null, value: boolean): VersionRangeTables {
    return new DeepMap([
      [{ type: 'Flavor', flavor } as FlavorAtom, new VersionRangeTable([], [value])],
      //make sure the truth table is exhaustive
      [{ type: 'FlavorNot', flavors: new Set([flavor]) } as FlavorAtom, new VersionRangeTable([], [false])],
    ]);
  }

  static cmpPoint(flavor: string | null, point: VersionRangePoint, left: boolean, right: boolean): VersionRangeTables {
    return new DeepMap([
      [{ type: 'Flavor', flavor } as FlavorAtom, new VersionRangeTable([point], [left, right])],
      // make sure the truth table is exhaustive
      [{ type: 'FlavorNot', flavors: new Set([flavor]) } as FlavorAtom, new VersionRangeTable([], [false])],
    ]);
  }

  static cmp(version: ExtendedVersion, side: -1 | 1, left: boolean, right: boolean): VersionRangeTables {
    return VersionRangeTable.cmpPoint(version.flavor, { upstream: version.upstream, downstream: version.downstream, side }, left, right)
  }

  static not(tables: VersionRangeTables) {
    if (tables === true || tables === false) {
      return !tables;
    }
    for (let [f, t] of tables) {
      for (let i = 0; i < t.values.length; i++) {
        t.values[i] = !t.values[i];
      }
    }
    return tables;
  }

  static and(a_tables: VersionRangeTables, b_tables: VersionRangeTables): VersionRangeTables {
    if (a_tables === true) {
      return b_tables;
    }
    if (b_tables === true) {
      return a_tables;
    }
    if (a_tables === false || b_tables == false) {
      return false;
    }
    let c_tables: VersionRangeTables = true;
    for (let [f_a, a] of a_tables) {
      for (let [f_b, b] of b_tables) {
        let flavor = flavorAnd(f_a, f_b);
        if (flavor == null) {
          continue;
        }
        let c = VersionRangeTable.zip(a, b, (a, b) => a && b);
        if (c_tables === true) {
          c_tables = new DeepMap();
        }
        let prev_c = c_tables.get(flavor);
        if (prev_c == null) {
          c_tables.set(flavor, c);
        } else {
          c_tables.set(flavor, VersionRangeTable.zip(c, prev_c, (a, b) => a || b));
        }
      }
    }
    return c_tables;
  }

  static or(...in_tables: VersionRangeTables[]): VersionRangeTables {
    let out_tables: VersionRangeTables = false;
    for (let tables of in_tables) {
      if (tables === false) {
        continue;
      }
      if (tables === true) {
        return true;
      }
      if (out_tables === false) {
        out_tables = new DeepMap();
      }
      for (let [flavor, table] of tables) {
        let prev = out_tables.get(flavor);
        if (prev == null) {
          out_tables.set(flavor, table);
        } else {
          out_tables.set(flavor, VersionRangeTable.zip(table, prev, (a, b) => a || b));
        }
      }
    }
    return out_tables;
  }

  static collapse(tables: VersionRangeTables): boolean | null {
    if (tables === true || tables === false) {
      return tables;
    } else {
      let found = null;
      for (let table of tables.values()) {
        for (let x of table.values) {
          if (found == null) {
            found = x;
          } else if (found != x) {
            return null;
          }
        }
      }
      return found;
    }
  }

  static miniterms(tables: VersionRangeTables): VersionRange {
    let collapse = VersionRangeTable.collapse(tables);
    if (tables === true || collapse === true) {
      return VersionRange.any()
    }
    if (tables == false || collapse === false) {
      return VersionRange.none()
    }
    let sum_terms: VersionRange[] = [];
    for (let [flavor, table] of tables) {
      let cmp_flavor = null;
      if (flavor.type == 'Flavor') {
        cmp_flavor = flavor.flavor;
      }
      for (let i = 0; i < table.values.length; i++) {
        let term: VersionRange[] = [];
        if (!table.values[i]) {
          continue
        }

        if (flavor.type == 'FlavorNot') {
          for (let not_flavor of flavor.flavors) {
            term.push(VersionRange.flavor(not_flavor).not());
          }
        }

        let p = null;
        let q = null;
        if (i > 0) {
          p = table.points[i - 1];
        }
        if (i < table.points.length) {
          q = table.points[i];
        }

        if (p != null && q != null && adjacentVersionRangePoints(p, q)) {
            term.push(VersionRange.anchor('=', new ExtendedVersion(cmp_flavor, p.upstream, p.downstream)));
        } else {
          if (p != null && p.side < 0) {
            term.push(VersionRange.anchor('>=', new ExtendedVersion(cmp_flavor, p.upstream, p.downstream)));
          }
          if (p != null && p.side >= 0)
            term.push(VersionRange.anchor('>', new ExtendedVersion(cmp_flavor, p.upstream, p.downstream)));
          }
          if (q != null && q.side < 0) {
            term.push(VersionRange.anchor('<', new ExtendedVersion(cmp_flavor, q.upstream, q.downstream)));
          if (q != null && q.side >= 0) {
            term.push(VersionRange.anchor('<=', new ExtendedVersion(cmp_flavor, q.upstream, q.downstream)));
          }
        }

        if (term.length == 0) {
          term.push(VersionRange.flavor(cmp_flavor));
        }

        sum_terms.push(VersionRange.and(...term));
      }
    }
    return VersionRange.or(...sum_terms);
  }
}

export class VersionRange {
  constructor(public atom: Anchor | And | Or | Not | P.Any | P.None | Flavor) {}

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
        return this.atom.flavor == null ? `#` : `#${this.atom.flavor}`
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
      case "Flavor":
        return VersionRange.flavor(atom.flavor)
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

  static anchor(operator: P.CmpOp, version: ExtendedVersion) {
    return new VersionRange({ type: "Anchor", operator, version })
  }

  static flavor(flavor: string | null) {
    return new VersionRange({ type: "Flavor", flavor })
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

  static and(...xs: Array<VersionRange>) {
    let y = VersionRange.any();
    for (let x of xs) {
      if (x.atom.type == 'Any') {
        continue;
      }
      if (x.atom.type == 'None') {
        return x;
      }
      if (y.atom.type == 'Any') {
        y = x;
      } else {
        y = new VersionRange({ type: 'And', left: y, right: x});
      }
    }
    return y;
  }

  static or(...xs: Array<VersionRange>) {
    let y = VersionRange.none();
    for (let x of xs) {
      if (x.atom.type == 'None') {
        continue;
      }
      if (x.atom.type == 'Any') {
        return x;
      }
      if (y.atom.type == 'None') {
        y = x;
      } else {
        y = new VersionRange({ type: 'Or', left: y, right: x});
      }
    }
    return y;
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

  tables(): VersionRangeTables {
    switch(this.atom.type) {
      case "Anchor":
        switch (this.atom.operator) {
          case "=":
            return VersionRangeTable.and(
              VersionRangeTable.cmp(this.atom.version, -1, false, true),
              VersionRangeTable.cmp(this.atom.version, 1, true, false),
            )
          case ">":
            return VersionRangeTable.cmp(this.atom.version, 1, false, true)
          case "<":
            return VersionRangeTable.cmp(this.atom.version, -1, true, false)
          case ">=":
            return VersionRangeTable.cmp(this.atom.version, -1, false, true)
          case "<=":
            return VersionRangeTable.cmp(this.atom.version, 1, true, false)
          case "!=":
            return VersionRangeTable.not(VersionRangeTable.and(
              VersionRangeTable.cmp(this.atom.version, -1, false, true),
              VersionRangeTable.cmp(this.atom.version, 1, true, false),
            ))
          case "^":
            return VersionRangeTable.and(
              VersionRangeTable.cmp(this.atom.version, -1, false, true),
              VersionRangeTable.cmp(this.atom.version.incrementMajor(), -1, true, false),
            )
          case "~":
            return VersionRangeTable.and(
              VersionRangeTable.cmp(this.atom.version, -1, false, true),
              VersionRangeTable.cmp(this.atom.version.incrementMinor(), -1, true, false),
            )
        }
      case "Flavor":
        return VersionRangeTable.full(this.atom.flavor, true)
      case "Not":
        return VersionRangeTable.not(this.atom.value.tables())
      case "And":
        return VersionRangeTable.and(this.atom.left.tables(), this.atom.right.tables())
      case "Or":
        return VersionRangeTable.or(this.atom.left.tables(), this.atom.right.tables())
      case "Any":
        return true
      case "None":
        return false
    }
  }

  satisfiable(): boolean {
    return VersionRangeTable.collapse(this.tables()) !== false;
  }

  intersects(other: VersionRange): boolean {
    return VersionRange.and(this, other).satisfiable();
  }

  normalize(): VersionRange {
    return VersionRangeTable.miniterms(this.tables());
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

  compareForSort(other: Version): -1 | 0 | 1 {
    switch (this.compare(other)) {
      case "greater":
        return 1
      case "equal":
        return 0
      case "less":
        return -1
    }
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
      case "Flavor":
        return versionRange.atom.flavor == this.flavor
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
