import { deepEqual } from "../util"
import { deepMerge } from "../util"

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
  test("Test that merging lists has Set semantics", () => {
    const merge = deepMerge(["a", "b"], ["b", "c"])
    expect(merge).toHaveLength(3)
    expect(merge).toContain("a")
    expect(merge).toContain("b")
    expect(merge).toContain("c")
  })
})
