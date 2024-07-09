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

export class VersionRange  {
  negator: boolean;
  operator: string;
  anchor: ExtendedVersion;

  constructor(range: string) {
    if (range.startsWith("!")) {
      this.negator = true
      range = range.replace("!", "")
    } else {
      this.negator = false
    }
    const operator = (() => {
      switch (range.substring(0, 2)) {
        case ">=": return ">="
        case "<=": return "<="
      }
      switch (range.substring(0, 1)) {
        case "=": return "="
        case "<": return "<"
        case ">": return ">"
      }
      throw new Error("Error parsing range:" + range)
    })();
    const comparisonOperators = /[><=]/g;
    const version = range.replace(comparisonOperators, "");
    this.operator = operator
    this.anchor = exverParser.parse(version)
  }

  public satisfiedBy(thisVersion: ExtendedVersion): boolean {
    if (this.negator) {
      if (this.anchor.flavor !== thisVersion.flavor) return true
      switch (this.operator) {
        case ">=": {
          return !(thisVersion.greaterThan(this.anchor) || thisVersion.equals(this.anchor))
        }
        case "<=": {
          return !(thisVersion.lessThan(this.anchor) || thisVersion.equals(this.anchor))
        }
        case ">": {
          return !thisVersion.greaterThan(this.anchor)
        }
        case "<": {
          return !thisVersion.lessThan(this.anchor)
        }
        case "=": {
          return !thisVersion.equals(this.anchor)
        }
      }
    } else {
      if (this.anchor.flavor !== thisVersion.flavor) return false
      switch (this.operator) {
        case ">=": {
          return thisVersion.greaterThan(this.anchor) || thisVersion.equals(this.anchor)
        }
        case "<=": {
          return thisVersion.lessThan(this.anchor) || thisVersion.equals(this.anchor)
        }
        case ">": {
          return thisVersion.greaterThan(this.anchor)
        }
        case "<": {
          return thisVersion.lessThan(this.anchor)
        }
        case "=": {
          return thisVersion.equals(this.anchor)
        }
      }
    }
    throw new Error("Error parsing range" + this.operator + this.anchor)
  }
}

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
  
  public toString(exver: ValidExtendedVersion): string {
    let exver_str = ""

    if (exver.flavor) {
      exver_str += '#';
      exver_str += exver.flavor;
      exver_str += ':';
    }

    exver_str = appendVersion(exver.upstream, exver_str);
    exver_str += ':';
    exver_str = appendVersion(exver.downstream, exver_str);

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
    if (this.equals(other)) {
      return 0 as const
    } else if (this.greaterThan(other)) {
      return 1 as const
    } else {
      return -1 as const
    }
  }
}

function greaterThan(thisVersion: ExtendedVersion, otherVersion: ExtendedVersion): boolean {
  for (const i in thisVersion.upstream.number) {
    if (otherVersion.upstream.number[i] == null) {
      return true
    }
    if (thisVersion.upstream.number[i] > otherVersion.upstream.number[i]) {
      return true
    }
    if (thisVersion.upstream.number[i] < otherVersion.upstream.number[i]) {
      return false
    }
  }
  for (const i in thisVersion.upstream.prerelease) {
    if (thisVersion.upstream.prerelease[i] > otherVersion.upstream.prerelease[i]) {
      return true
    } else if (thisVersion.upstream.prerelease[i] < otherVersion.upstream.prerelease[i]) {
      return false
    }
  }
  for (const i in thisVersion.downstream.number) {
    if (otherVersion.downstream.number[i] == null) {
      return true
    }
    if (thisVersion.downstream.number[i] > otherVersion.downstream.number[i]) {
      return true
    }
    if (thisVersion.downstream.number[i] < otherVersion.downstream.number[i]) {
      return false
    }
  }
  for (const i in thisVersion.upstream.prerelease) {
    if (thisVersion.upstream.prerelease[i] > otherVersion.upstream.prerelease[i]) {
      return true
    } else if (thisVersion.upstream.prerelease[i] < otherVersion.upstream.prerelease[i]) {
      return false
    }
  }
  return false
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
    if (i < version.number.length) {
      str += '.';
    }
  });

  if (version.prerelease.length > 0) {
    version.prerelease.forEach((n, i) => {
      if (typeof n === 'string') {
        str += '-';
      }
      str += n;
      if (i < version.prerelease.length) {
        str += '.';
      }
    })
  }

  return str;
}

export function satisfiedByMany(range: string, thisVersion: ExtendedVersion): boolean {
  if (range.includes("&&")) {
    const ranges = range.split("&&");
    for (const x of ranges) {
      console.log(x.trim())
      const versionRange = new VersionRange(x.trim())
      if (!versionRange.satisfiedBy(thisVersion)) {
        return false
      }
    }
    return true
  }
  if (range.includes("||")) {
    const ranges = range.split("||");
    for (const x of ranges) {
      const versionRange = new VersionRange(x.trim())
      if (versionRange.satisfiedBy(thisVersion)) {
        return true
      }
    }
    return false
  }
  throw new Error("Couldn't parse range: " + range)
}