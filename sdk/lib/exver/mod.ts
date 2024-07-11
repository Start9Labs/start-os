const exverParser = require('./exver.js');

export interface ValidExtendedVersion {
  flavor: string | null,
  upstream: Version,
  downstream: Version,
}

export interface Version {
  number: number[],
  prerelease: (string | number)[],
}

// !( >=1:1 && <= 2:2)
type Operator = ">" | "<" | ">=" | "<=" | "!="

interface Anchor {
  type: "Anchor",
  operator: Operator,
  version: ExtendedVersion,
}

interface And {
  type: "And",
  left: VersionRange,
  right: VersionRange,
}

interface Or {
  type: "Or",
  left: VersionRange,
  right: VersionRange,
}

interface Not {
  type: "Not",
  value: VersionRange,
}

interface Any {
  type: "Any"
}

interface None {
  type: "None"
}

// type VersionRange = Anchor | And | Or | Not | Any | None

abstract class VersionRange {
  type: string
  constructor (type: string) {
    this.type = type
  }
}

class Anchor extends VersionRange {
  operator: Operator;
  version: ExtendedVersion;

  constructor(operator: Operator, version: ExtendedVersion) {
    super("Anchor")
    this.operator = operator;
    this.version = version;
  }
}

class And extends VersionRange {
  right: VersionRange;
  left: VersionRange;

  constructor(left: VersionRange, right: VersionRange) {
    super("And")
    this.left = left;
    this.right =right;
  }
}

class Or extends VersionRange {
  right: VersionRange;
  left: VersionRange;

  constructor(left: VersionRange, right: VersionRange) {
    super("Or")
    this.left = left;
    this.right =right;
  }
}

class Not extends VersionRange {
  value: VersionRange;

  constructor(version: VersionRange) {
    super("Not")
    this.value = version;
  }
}

class Any extends VersionRange {
  constructor() {
    super("Any")
  }
}

class None extends VersionRange {
  constructor() {
    super("None")
  }
}

class VersionRangeConstructor {
  static parseVersionRange(range: VersionRange): VersionRange {
    switch (range.type) {
      case "Anchor":
        return new Anchor(
          (range as Anchor).operator,
          (range as Anchor).version
        )
        // return new Anchor(range.operator, range.version);
      case "And":
        return new And(
          this.parseVersionRange((range as And).left),
          this.parseVersionRange((range as And).right),
          // this.parseVersionRange(range.left),
          // this.parseVersionRange(range.right),
        );
      case "Or":
        return new Or(
          this.parseVersionRange((range as Or).left),
          this.parseVersionRange((range as Or).right),
          // this.parseVersionRange(range.left),
          // this.parseVersionRange(range.right),
        );
      case "Any":
        return new Any();
      case "None":
        return new None();
      case "Not":
        return new Not(
          this.parseVersionRange((range as Not).value)
          // this.parseVersionRange(range.value)
        );
      default:
        throw new Error(`Unknown type: ${range.type}`)
    }
  }
}
// export class VersionRange  {
//   negator: boolean;
//   operator: string;
//   anchor: ExtendedVersion;

//   constructor(range: string) {
//     if (range.startsWith("!")) {
//       this.negator = true
//       range = range.replace("!", "")
//     } else {
//       this.negator = false
//     }
//     const operator = (() => {
//       switch (range.substring(0, 2)) {
//         case ">=": return ">="
//         case "<=": return "<="
//       }
//       switch (range.substring(0, 1)) {
//         case "=": return "="
//         case "<": return "<"
//         case ">": return ">"
//       }
//       throw new Error("Error parsing range:" + range)
//     })();
//     const comparisonOperators = /[><=]/g;
//     const version = range.replace(comparisonOperators, "");
//     this.operator = operator
//     this.anchor = exverParser.parse(version)
//   }

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
  flavor: string | null;
  upstream: Version;
  downstream: Version;

  constructor(
    exverString: string
  ) {
    const exver = exverParser.parse(exverString)
    this.flavor = exver.flavor;
    this.upstream = exver.upstream;
    this.downstream = exver.downstream;
  }
  
  toString(): string {
    let exver_str = ""

    if (this.flavor) {
      exver_str += '#';
      exver_str += this.flavor;
      exver_str += ':';
    }

    exver_str = appendVersion(this.upstream, exver_str);
    exver_str += ':';
    exver_str = appendVersion(this.downstream, exver_str);

    return exver_str;
  }

  public greaterThan(other: ExtendedVersion): boolean {
    return greaterThan(this, other)
  }

  public equals(other: ExtendedVersion): boolean {
    return equals(this.upstream, other.upstream) && equals(this.downstream, other.downstream)
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

function greaterThan(thisVersion: ExtendedVersion, otherVersion: ExtendedVersion): boolean {
  const upstreamGtResult = versionGt(thisVersion.upstream, otherVersion.upstream)
  if (typeof upstreamGtResult === 'boolean') {
    return upstreamGtResult
  }
  const downstreamGtResult = versionGt(thisVersion.downstream, otherVersion.downstream)
  if (typeof downstreamGtResult === 'boolean') {
    return downstreamGtResult
  }
  return false
}

function versionGt(thisVersion: Version, otherVersion: Version): boolean | undefined {
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
    str += n;
    if (i < version.number.length -1 ) {
      str += '.';
    }
  });

  if (version.prerelease.length > 0) {
    str += '-';
    version.prerelease.forEach((n, i) => {
      str += n;
      if (i < version.prerelease.length -1) {
        str += '.';
      }
    })
  }

  return str;
}

// export function satisfiedByMany(range: string, thisVersion: ExtendedVersion): boolean {
//   if (range.includes("&&")) {
//     const ranges = range.split("&&");
//     for (const x of ranges) {
//       console.log(x.trim())
//       const versionRange = new VersionRange(x.trim())
//       if (!versionRange.satisfiedBy(thisVersion)) {
//         return false
//       }
//     }
//     return true
//   }
//   if (range.includes("||")) {
//     const ranges = range.split("||");
//     for (const x of ranges) {
//       const versionRange = new VersionRange(x.trim())
//       if (versionRange.satisfiedBy(thisVersion)) {
//         return true
//       }
//     }
//     return false
//   }
//   throw new Error("Couldn't parse range: " + range)
// }

// const ex1 = new VersionRange(">=#bitcoin:1.0.0-alpha:0 && <=#bitcoin:1.2.0-alpha:0")
// const ex2 = new VersionRange(">=#bitcoin:1.0.0-alpha:0")

const ex1 = exverParser.parse("!( >= 1:1 <= 2:2) || <=#bitcoin:1.2.0-alpha:0") as VersionRange
const parsedRange = VersionRangeConstructor.parseVersionRange(ex1);
console.log(parsedRange)
console.log("ETC")
if (parsedRange.type === "Not") {
  console.log(parsedRange.value)
  if (parsedRange.value.type === "And") {
    if (parsedRange.value.left.type === "Anchor") {
      console.log(parsedRange.value.left.version.upstream)
      console.log(parsedRange.value.left.version.downstream)
    }
    if (parsedRange.value.right.type === "Anchor") {
      console.log(parsedRange.value.right.version.upstream)
      console.log(parsedRange.value.right.version.downstream)
    }
  }
}