import { deepEqual } from "../util/deepEqual"
import { deepMerge } from "../util/deepMerge"

describe("deepMerge", () => {
  test("deepMerge({}, {a: 1}, {b: 2}) should return {a: 1, b: 2}", () => {
    expect(deepMerge({}, { a: 1 }, { b: 2 })).toEqual({ a: 1, b: 2 })
  })
  test("deepMerge(null, [1,2,3]) should equal [1,2,3]", () => {
    expect(deepMerge(null, [1, 2, 3])).toEqual([1, 2, 3])
  })
  test("deepMerge({a: {b: 1, c:2}}, {a: {b: 3}}) should equal {a: {b: 3, c: 2}}", () => {
    expect(deepMerge({ a: { b: 1, c: 2 } }, { a: { b: 3 } })).toEqual({
      a: { b: 3, c: 2 },
    })
  })
  test("deepMerge({a: {b: 1, c:2}}, {a: {b: 3}}) should equal {a: {b: 3, c: 2}} with deep equal", () => {
    expect(
      deepEqual(deepMerge({ a: { b: 1, c: 2 } }, { a: { b: 3 } }), {
        a: { b: 3, c: 2 },
      }),
    ).toBeTruthy()
  })
  test("deepMerge([1,2,3], [2,3,4]) should equal [2,3,4]", () => {
    expect(deepMerge([1, 2, 3], [2, 3, 4])).toEqual([2, 3, 4])
  })
})
