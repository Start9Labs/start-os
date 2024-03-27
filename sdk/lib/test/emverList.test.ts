import { EmVer, notRange, rangeAnd, rangeOf, rangeOr } from "../emverLite/mod"
describe("EmVer", () => {
  {
    {
      const checker = rangeOf("*")
      test("rangeOf('*')", () => {
        checker.check("1")
        checker.check("1.2")
        checker.check("1.2.3")
        checker.check("1.2.3.4")
        // @ts-expect-error
        checker.check("1.2.3.4.5")
        // @ts-expect-error
        checker.check("1.2.3.4.5.6")
        expect(checker.check("1")).toEqual(true)
        expect(checker.check("1.2")).toEqual(true)
        expect(checker.check("1.2.3.4")).toEqual(true)
      })
      test("rangeOf('*') invalid", () => {
        // @ts-expect-error
        expect(() => checker.check("a")).toThrow()
        // @ts-expect-error
        expect(() => checker.check("")).toThrow()
        expect(() => checker.check("1..3")).toThrow()
      })
    }

    {
      const checker = rangeOf(">1.2.3.4")
      test(`rangeOf(">1.2.3.4") valid`, () => {
        expect(checker.check("2-beta123")).toEqual(true)
        expect(checker.check("2")).toEqual(true)
        expect(checker.check("1.2.3.5")).toEqual(true)
        // @ts-expect-error
        expect(checker.check("1.2.3.4.1")).toEqual(true)
      })

      test(`rangeOf(">1.2.3.4") invalid`, () => {
        expect(checker.check("1.2.3.4")).toEqual(false)
        expect(checker.check("1.2.3")).toEqual(false)
        expect(checker.check("1")).toEqual(false)
      })
    }
    {
      const checker = rangeOf("=1.2.3")
      test(`rangeOf("=1.2.3") valid`, () => {
        expect(checker.check("1.2.3")).toEqual(true)
      })

      test(`rangeOf("=1.2.3") invalid`, () => {
        expect(checker.check("2")).toEqual(false)
        expect(checker.check("1.2.3.1")).toEqual(false)
        expect(checker.check("1.2")).toEqual(false)
      })
    }
    {
      const checker = rangeOf(">=1.2.3.4")
      test(`rangeOf(">=1.2.3.4") valid`, () => {
        expect(checker.check("2")).toEqual(true)
        expect(checker.check("1.2.3.5")).toEqual(true)
        // @ts-expect-error
        expect(checker.check("1.2.3.4.1")).toEqual(true)
        expect(checker.check("1.2.3.4")).toEqual(true)
      })

      test(`rangeOf(">=1.2.3.4") invalid`, () => {
        expect(checker.check("1.2.3")).toEqual(false)
        expect(checker.check("1")).toEqual(false)
      })
    }
    {
      const checker = rangeOf("<1.2.3.4")
      test(`rangeOf("<1.2.3.4") invalid`, () => {
        expect(checker.check("2")).toEqual(false)
        expect(checker.check("1.2.3.5")).toEqual(false)
        // @ts-expect-error
        expect(checker.check("1.2.3.4.1")).toEqual(false)
        expect(checker.check("1.2.3.4")).toEqual(false)
      })

      test(`rangeOf("<1.2.3.4") valid`, () => {
        expect(checker.check("1.2.3")).toEqual(true)
        expect(checker.check("1")).toEqual(true)
      })
    }
    {
      const checker = rangeOf("<=1.2.3.4")
      test(`rangeOf("<=1.2.3.4") invalid`, () => {
        expect(checker.check("2")).toEqual(false)
        expect(checker.check("1.2.3.5")).toEqual(false)
        // @ts-expect-error
        expect(checker.check("1.2.3.4.1")).toEqual(false)
      })

      test(`rangeOf("<=1.2.3.4") valid`, () => {
        expect(checker.check("1.2.3")).toEqual(true)
        expect(checker.check("1")).toEqual(true)
        expect(checker.check("1.2.3.4")).toEqual(true)
      })
    }

    {
      const checkA = rangeOf(">1")
      const checkB = rangeOf("<=2")

      const checker = rangeAnd(checkA, checkB)
      test(`simple and(checkers) valid`, () => {
        expect(checker.check("2")).toEqual(true)

        expect(checker.check("1.1")).toEqual(true)
      })
      test(`simple and(checkers) invalid`, () => {
        expect(checker.check("2.1")).toEqual(false)
        expect(checker.check("1")).toEqual(false)
        expect(checker.check("0")).toEqual(false)
      })
    }
    {
      const checkA = rangeOf("<1")
      const checkB = rangeOf("=2")

      const checker = rangeOr(checkA, checkB)
      test(`simple or(checkers) valid`, () => {
        expect(checker.check("2")).toEqual(true)
        expect(checker.check("0.1")).toEqual(true)
      })
      test(`simple or(checkers) invalid`, () => {
        expect(checker.check("2.1")).toEqual(false)
        expect(checker.check("1")).toEqual(false)
        expect(checker.check("1.1")).toEqual(false)
      })
    }

    {
      const checker = rangeOf("1.2.*")
      test(`rangeOf(1.2.*) valid`, () => {
        expect(checker.check("1.2")).toEqual(true)
        expect(checker.check("1.2.1")).toEqual(true)
      })
      test(`rangeOf(1.2.*) invalid`, () => {
        expect(checker.check("1.3")).toEqual(false)
        expect(checker.check("1.3.1")).toEqual(false)

        expect(checker.check("1.1.1")).toEqual(false)
        expect(checker.check("1.1")).toEqual(false)
        expect(checker.check("1")).toEqual(false)

        expect(checker.check("2")).toEqual(false)
      })
    }

    {
      const checker = notRange(rangeOf("1.2.*"))
      test(`notRange(rangeOf(1.2.*)) valid`, () => {
        expect(checker.check("1.3")).toEqual(true)
        expect(checker.check("1.3.1")).toEqual(true)

        expect(checker.check("1.1.1")).toEqual(true)
        expect(checker.check("1.1")).toEqual(true)
        expect(checker.check("1")).toEqual(true)

        expect(checker.check("2")).toEqual(true)
      })
      test(`notRange(rangeOf(1.2.*)) invalid `, () => {
        expect(checker.check("1.2")).toEqual(false)
        expect(checker.check("1.2.1")).toEqual(false)
      })
    }
    {
      const checker = rangeOf("!1.2.*")
      test(`!(rangeOf(1.2.*)) valid`, () => {
        expect(checker.check("1.3")).toEqual(true)
        expect(checker.check("1.3.1")).toEqual(true)

        expect(checker.check("1.1.1")).toEqual(true)
        expect(checker.check("1.1")).toEqual(true)
        expect(checker.check("1")).toEqual(true)

        expect(checker.check("2")).toEqual(true)
      })
      test(`!(rangeOf(1.2.*)) invalid `, () => {
        expect(checker.check("1.2")).toEqual(false)
        expect(checker.check("1.2.1")).toEqual(false)
      })
    }
    {
      test(`no and ranges`, () => {
        expect(() => rangeAnd()).toThrow()
      })
      test(`no or ranges`, () => {
        expect(() => rangeOr()).toThrow()
      })
    }
    {
      const checker = rangeOf("!>1.2.3.4")
      test(`rangeOf("!>1.2.3.4") invalid`, () => {
        expect(checker.check("2")).toEqual(false)
        expect(checker.check("1.2.3.5")).toEqual(false)
        // @ts-expect-error
        expect(checker.check("1.2.3.4.1")).toEqual(false)
      })

      test(`rangeOf("!>1.2.3.4") valid`, () => {
        expect(checker.check("1.2.3.4")).toEqual(true)
        expect(checker.check("1.2.3")).toEqual(true)
        expect(checker.check("1")).toEqual(true)
      })
    }

    {
      test(">1 && =1.2", () => {
        const checker = rangeOf(">1 && =1.2")

        expect(checker.check("1.2")).toEqual(true)
        expect(checker.check("1.2.1")).toEqual(false)
      })
      test("=1 || =2", () => {
        const checker = rangeOf("=1 || =2")

        expect(checker.check("1")).toEqual(true)
        expect(checker.check("2")).toEqual(true)
        expect(checker.check("3")).toEqual(false)
      })

      test(">1 && =1.2 || =2", () => {
        const checker = rangeOf(">1 && =1.2 || =2")

        expect(checker.check("1.2")).toEqual(true)
        expect(checker.check("1")).toEqual(false)
        expect(checker.check("2")).toEqual(true)
        expect(checker.check("3")).toEqual(false)
      })

      test("&& before || order of operationns:  <1.5 && >1 || >1.5 && <3", () => {
        const checker = rangeOf("<1.5 && >1 || >1.5 && <3")
        expect(checker.check("1.1")).toEqual(true)
        expect(checker.check("2")).toEqual(true)

        expect(checker.check("1.5")).toEqual(false)
        expect(checker.check("1")).toEqual(false)
        expect(checker.check("3")).toEqual(false)
      })

      test("Compare function on the emver", () => {
        const a = EmVer.from("1.2.3")
        const b = EmVer.from("1.2.4")

        expect(a.compare(b)).toEqual("less")
        expect(b.compare(a)).toEqual("greater")
        expect(a.compare(a)).toEqual("equal")
      })
      test("Compare for sort function on the emver", () => {
        const a = EmVer.from("1.2.3")
        const b = EmVer.from("1.2.4")

        expect(a.compareForSort(b)).toEqual(-1)
        expect(b.compareForSort(a)).toEqual(1)
        expect(a.compareForSort(a)).toEqual(0)
      })
    }
  }
})
