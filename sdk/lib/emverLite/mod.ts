import * as matches from "ts-matches"

const starSub = /((\d+\.)*\d+)\.\*/

type Decrement = [never, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
//
// prettier-ignore
export type ValidateVersion<T extends String> = 
T extends `-${infer A}` ? never  :
T extends `${infer A}-${infer B}` ? ValidateVersion<A> & ValidateVersion<B> :
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
  T extends [] ? unknown :
  T extends [infer A, ...infer B] ? ValidateExVer<A & string> & ValidateExVers<B> :
  never

// prettier-ignore
export type ValidExVerRangeDep = string;

function incrementLastNumber(list: number[]) {
  const newList = [...list]
  newList[newList.length - 1]++
  return newList
}
/**
 * Will take in a range, like `>1.2` or `<1.2.3.4` or `=1.2` or `1.*`
 * and return a checker, that has the check function for checking that a version is in the valid
 * @param range
 * @returns
 */
export function rangeOf(range: string | Checker): Checker {
  return Checker.parse(range)
}

/**
 * Used to create a checker that will `and` all the ranges passed in
 * @param ranges
 * @returns
 */
export function rangeAnd(...ranges: (string | Checker)[]): Checker {
  if (ranges.length === 0) {
    throw new Error("No ranges given")
  }
  const [firstCheck, ...rest] = ranges
  return Checker.parse(firstCheck).and(...rest)
}

/**
 * Used to create a checker that will `or` all the ranges passed in
 * @param ranges
 * @returns
 */
export function rangeOr(...ranges: (string | Checker)[]): Checker {
  if (ranges.length === 0) {
    throw new Error("No ranges given")
  }
  const [firstCheck, ...rest] = ranges
  return Checker.parse(firstCheck).or(...rest)
}

/**
 * This will negate the checker, so given a checker that checks for >= 1.0.0, it will check for < 1.0.0
 * @param range
 * @returns
 */
export function notRange(range: string | Checker): Checker {
  return rangeOf(range).not()
}

/**
 * EmVer is a set of versioning of any pattern like 1 or 1.2 or 1.2.3 or 1.2.3.4 or ..
 */
export class EmVer {
  /**
   * Convert the range, should be 1.2.* or * into a emver
   * Or an already made emver
   * IsUnsafe
   */
  static from(range: string | EmVer): EmVer {
    if (range instanceof EmVer) {
      return range
    }
    return EmVer.parse(range)
  }
  /**
   * Convert the range, should be 1.2.* or * into a emver
   * IsUnsafe
   */
  static parse(rangeExtra: string): EmVer {
    const [range, extra] = rangeExtra.split("-")
    const values = range.split(".").map((x) => parseInt(x))
    for (const value of values) {
      if (isNaN(value)) {
        throw new Error(`Couldn't parse range: ${range}`)
      }
    }
    const extraValue = extra ? parseInt(extra) : null
    if (!!extraValue && isNaN(extraValue)) {
      throw new Error(`Couldn't parse range: ${extra}`)
    }
    return new EmVer(values, extraValue)
  }
  private constructor(
    public readonly values: number[],
    readonly extra: number | null,
  ) {}

  /**
   * Used when we need a new emver that has the last number incremented, used in the 1.* like things
   */
  public withLastIncremented() {
    return new EmVer(incrementLastNumber(this.values), null)
  }

  public greaterThan(other: EmVer): boolean {
    for (const i in this.values) {
      if (other.values[i] == null) {
        return true
      }
      if (this.values[i] > other.values[i]) {
        return true
      }

      if (this.values[i] < other.values[i]) {
        return false
      }
    }
    return false
  }

  public equals(other: EmVer): boolean {
    if (other.values.length !== this.values.length) {
      return false
    }
    for (const i in this.values) {
      if (this.values[i] !== other.values[i]) {
        return false
      }
    }
    return true
  }
  public greaterThanOrEqual(other: EmVer): boolean {
    return this.greaterThan(other) || this.equals(other)
  }
  public lessThanOrEqual(other: EmVer): boolean {
    return !this.greaterThan(other)
  }
  public lessThan(other: EmVer): boolean {
    return !this.greaterThanOrEqual(other)
  }
  /**
   * Return a enum string that describes (used for switching/iffs)
   * to know comparison
   * @param other
   * @returns
   */
  public compare(other: EmVer) {
    if (this.equals(other)) {
      return "equal" as const
    } else if (this.greaterThan(other)) {
      return "greater" as const
    } else {
      return "less" as const
    }
  }
  /**
   * Used when sorting emver's in a list using the sort method
   * @param other
   * @returns
   */
  public compareForSort(other: EmVer) {
    return matches
      .matches(this.compare(other))
      .when("equal", () => 0 as const)
      .when("greater", () => 1 as const)
      .when("less", () => -1 as const)
      .unwrap()
  }

  toString() {
    const value =
      `${this.values.join(".") as `0.0`}${this.extra ? (`-${this.extra}` as const) : ""}` as const
    return testTypeVersion(value)
  }
}

/**
 * A checker is a function that takes a version and returns true if the version matches the checker.
 * Used when we are doing range checking, like saying ">=1.0.0".check("1.2.3") will be true
 */
export class Checker {
  /**
   * Will take in a range, like `>1.2` or `<1.2.3.4` or `=1.2` or `1.*`
   * and return a checker, that has the check function for checking that a version is in the valid
   * @param range
   * @returns
   */
  static parse(range: string | Checker): Checker {
    if (range instanceof Checker) {
      return range
    }
    range = range.trim()
    if (range.indexOf("||") !== -1) {
      return rangeOr(...range.split("||").map((x) => Checker.parse(x)))
    }
    if (range.indexOf("&&") !== -1) {
      return rangeAnd(...range.split("&&").map((x) => Checker.parse(x)))
    }
    if (range === "*") {
      return new Checker((version) => {
        EmVer.from(version)
        return true
      }, range)
    }
    if (range.startsWith("!!")) return Checker.parse(range.substring(2))
    if (range.startsWith("!")) {
      const tempValue = Checker.parse(range.substring(1))
      return new Checker((x) => !tempValue.check(x), range)
    }
    const starSubMatches = starSub.exec(range)
    if (starSubMatches != null) {
      const emVarLower = EmVer.parse(starSubMatches[1])
      const emVarUpper = emVarLower.withLastIncremented()

      return new Checker((version) => {
        const v = EmVer.from(version)
        return (
          (v.greaterThan(emVarLower) || v.equals(emVarLower)) &&
          !v.greaterThan(emVarUpper) &&
          !v.equals(emVarUpper)
        )
      }, range)
    }

    switch (range.substring(0, 2)) {
      case ">=": {
        const emVar = EmVer.parse(range.substring(2))
        return new Checker((version) => {
          const v = EmVer.from(version)
          return v.greaterThanOrEqual(emVar)
        }, range)
      }
      case "<=": {
        const emVar = EmVer.parse(range.substring(2))
        return new Checker((version) => {
          const v = EmVer.from(version)
          return v.lessThanOrEqual(emVar)
        }, range)
      }
    }

    switch (range.substring(0, 1)) {
      case ">": {
        const emVar = EmVer.parse(range.substring(1))
        return new Checker((version) => {
          const v = EmVer.from(version)
          return v.greaterThan(emVar)
        }, range)
      }
      case "<": {
        const emVar = EmVer.parse(range.substring(1))
        return new Checker((version) => {
          const v = EmVer.from(version)
          return v.lessThan(emVar)
        }, range)
      }
      case "=": {
        const emVar = EmVer.parse(range.substring(1))
        return new Checker((version) => {
          const v = EmVer.from(version)
          return v.equals(emVar)
        }, `=${emVar.toString()}`)
      }
    }
    throw new Error("Couldn't parse range: " + range)
  }
  constructor(
    /**
     * Check is the function that will be given a emver or unparsed emver and should give if it follows
     * a pattern
     */
    public readonly check: <A extends string = "">(
      value: EmVer | (A & ValidateExVer<A>),
    ) => boolean,
    private readonly _range: string,
  ) {}

  get range() {
    return this._range as ValidExVerRangeDep
  }

  /**
   * Used when we want the `and` condition with another checker
   */
  public and(...others: (Checker | string)[]): Checker {
    const othersCheck = others.map(Checker.parse)
    return new Checker(
      (value) => {
        if (!this.check(value)) {
          return false
        }
        for (const other of othersCheck) {
          if (!other.check(value)) {
            return false
          }
        }
        return true
      },
      othersCheck.map((x) => x._range).join(" && "),
    )
  }

  /**
   * Used when we want the `or` condition with another checker
   */
  public or(...others: (Checker | string)[]): Checker {
    const othersCheck = others.map(Checker.parse)
    return new Checker(
      (value) => {
        if (this.check(value)) {
          return true
        }
        for (const other of othersCheck) {
          if (other.check(value)) {
            return true
          }
        }
        return false
      },
      othersCheck.map((x) => x._range).join(" || "),
    )
  }

  /**
   * A useful example is making sure we don't match an exact version, like !=1.2.3
   * @returns
   */
  public not(): Checker {
    let newRange = `!${this._range}`
    return Checker.parse(newRange)
  }
}
export const testTypeEmverLite = <T extends string>(t: T & ValidateExVer<T>) =>
  t

export const testTypeVersion = <T extends string>(t: T & ValidateVersion<T>) =>
  t
function tests() {
  testTypeVersion("1.2.3")
  testTypeVersion("1")
  testTypeVersion("12.34.56")
  testTypeVersion("1.2-3")
  testTypeVersion("1-3")
  // @ts-expect-error
  testTypeVersion("-3")
  // @ts-expect-error
  testTypeVersion("1.2.3:1")
  // @ts-expect-error
  testTypeVersion("#cat:1:1")

  testTypeEmverLite("1.2.3:1.2.3")
  testTypeEmverLite("1.2.3.4.5.6.7.8.9.0:1")
  testTypeEmverLite("100:1")
  testTypeEmverLite("#cat:1:1")
  testTypeEmverLite("1.2.3.4.5.6.7.8.9.11.22.33:1")
  testTypeEmverLite("1-0:1")
  testTypeEmverLite("1-0:1")
  // @ts-expect-error
  testTypeEmverLite("1.2-3")
  // @ts-expect-error
  testTypeEmverLite("1-3")
  // @ts-expect-error
  testTypeEmverLite("1.2.3.4.5.6.7.8.9.0.10:1" as string)
  // @ts-expect-error
  testTypeEmverLite("1.-2:1")
  // @ts-expect-error
  testTypeEmverLite("1..2.3:3")
}
