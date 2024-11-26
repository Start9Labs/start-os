import { VersionRange, ExtendedVersion } from "../exver"
describe("ExVer", () => {
  {
    {
      const checker = VersionRange.parse("*")
      test("VersionRange.parse('*')", () => {
        checker.satisfiedBy(ExtendedVersion.parse("1:0"))
        checker.satisfiedBy(ExtendedVersion.parse("1.2:0"))
        checker.satisfiedBy(ExtendedVersion.parse("1.2.3:0"))
        checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4"))
        checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4.5"))
        checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4.5.6"))
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4"))).toEqual(
          true,
        )
      })
      test("VersionRange.parse('*') invalid", () => {
        expect(() => checker.satisfiedBy(ExtendedVersion.parse("a"))).toThrow()
        expect(() => checker.satisfiedBy(ExtendedVersion.parse(""))).toThrow()
        expect(() =>
          checker.satisfiedBy(ExtendedVersion.parse("1..3")),
        ).toThrow()
      })
    }

    {
      const checker = VersionRange.parse(">1.2.3:4")
      test(`VersionRange.parse(">1.2.3:4") valid`, () => {
        expect(
          checker.satisfiedBy(ExtendedVersion.parse("2-beta.123:0")),
        ).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:5"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4.1"))).toEqual(
          true,
        )
      })

      test(`VersionRange.parse(">1.2.3:4") invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(false)
      })
    }
    {
      const checker = VersionRange.parse("=1.2.3")
      test(`VersionRange.parse("=1.2.3") valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:0"))).toEqual(
          true,
        )
      })

      test(`VersionRange.parse("=1.2.3") invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(false)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:1"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2:0"))).toEqual(
          false,
        )
      })
    }
    {
      const checker = VersionRange.parse(">=1.2.3:4")
      test(`VersionRange.parse(">=1.2.3:4") valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:5"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4.1"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4"))).toEqual(
          true,
        )
      })

      test(`VersionRange.parse(">=1.2.3:4") invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(false)
      })
    }
    {
      const checker = VersionRange.parse("<1.2.3:4")
      test(`VersionRange.parse("<1.2.3:4") invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(false)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:5"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4.1"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4"))).toEqual(
          false,
        )
      })

      test(`VersionRange.parse("<1.2.3:4") valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(true)
      })
    }
    {
      const checker = VersionRange.parse("<=1.2.3:4")
      test(`VersionRange.parse("<=1.2.3:4") invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(false)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:5"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4.1"))).toEqual(
          false,
        )
      })

      test(`VersionRange.parse("<=1.2.3:4") valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4"))).toEqual(
          true,
        )
      })
    }

    {
      const checkA = VersionRange.parse(">1")
      const checkB = VersionRange.parse("<=2")

      const checker = checkA.and(checkB)
      test(`simple and(checkers) valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)

        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1:0"))).toEqual(
          true,
        )
      })
      test(`simple and(checkers) invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2.1:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(false)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(false)
      })
    }
    {
      const checkA = VersionRange.parse("<1")
      const checkB = VersionRange.parse("=2")

      const checker = checkA.or(checkB)
      test(`simple or(checkers) valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("0.1:0"))).toEqual(
          true,
        )
      })
      test(`simple or(checkers) invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2.1:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(false)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1:0"))).toEqual(
          false,
        )
      })
    }

    {
      const checker = VersionRange.parse("~1.2")
      test(`VersionRange.parse(~1.2) valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.1:0"))).toEqual(
          true,
        )
      })
      test(`VersionRange.parse(~1.2) invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.3:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.3.1:0"))).toEqual(
          false,
        )

        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1.1:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(false)

        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(false)
      })
    }

    {
      const checker = VersionRange.parse("~1.2").not()
      test(`VersionRange.parse(~1.2).not() valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.3:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.3.1:0"))).toEqual(
          true,
        )

        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1.1:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(true)

        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)
      })
      test(`VersionRange.parse(~1.2).not() invalid `, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.1:0"))).toEqual(
          false,
        )
      })
    }
    {
      const checker = VersionRange.parse("!~1.2")
      test(`!(VersionRange.parse(~1.2)) valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.3:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.3.1:0"))).toEqual(
          true,
        )

        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1.1:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(true)

        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)
      })
      test(`!(VersionRange.parse(~1.2)) invalid `, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.1:0"))).toEqual(
          false,
        )
      })
    }
    {
      const checker = VersionRange.parse("!>1.2.3:4")
      test(`VersionRange.parse("!>1.2.3:4") invalid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(false)
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:5"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4.1"))).toEqual(
          false,
        )
      })

      test(`VersionRange.parse("!>1.2.3:4") valid`, () => {
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:4"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.3:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(true)
      })
    }

    {
      test(">1 && =1.2", () => {
        const checker = VersionRange.parse(">1 && =1.2")

        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2.1:0"))).toEqual(
          false,
        )
      })
      test("=1 || =2", () => {
        const checker = VersionRange.parse("=1 || =2")

        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("3:0"))).toEqual(false)
      })

      test(">1 && =1.2 || =2", () => {
        const checker = VersionRange.parse(">1 && =1.2 || =2")

        expect(checker.satisfiedBy(ExtendedVersion.parse("1.2:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(false)
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)
        expect(checker.satisfiedBy(ExtendedVersion.parse("3:0"))).toEqual(false)
      })

      test("&& before || order of operationns:  <1.5 && >1 || >1.5 && <3", () => {
        const checker = VersionRange.parse("<1.5 && >1 || >1.5 && <3")
        expect(checker.satisfiedBy(ExtendedVersion.parse("1.1:0"))).toEqual(
          true,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("2:0"))).toEqual(true)

        expect(checker.satisfiedBy(ExtendedVersion.parse("1.5:0"))).toEqual(
          false,
        )
        expect(checker.satisfiedBy(ExtendedVersion.parse("1:0"))).toEqual(false)
        expect(checker.satisfiedBy(ExtendedVersion.parse("3:0"))).toEqual(false)
      })

      test("Compare function on the emver", () => {
        const a = ExtendedVersion.parse("1.2.3:0")
        const b = ExtendedVersion.parse("1.2.4:0")

        expect(a.compare(b)).toEqual("less")
        expect(b.compare(a)).toEqual("greater")
        expect(a.compare(a)).toEqual("equal")
      })
      test("Compare for sort function on the emver", () => {
        const a = ExtendedVersion.parse("1.2.3:0")
        const b = ExtendedVersion.parse("1.2.4:0")

        expect(a.compareForSort(b)).toEqual(-1)
        expect(b.compareForSort(a)).toEqual(1)
        expect(a.compareForSort(a)).toEqual(0)
      })
    }
  }
})
