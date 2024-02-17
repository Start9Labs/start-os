import { getHostname } from "../util/getNetworkInterface"
import { splitCommand } from "../util/splitCommand"

describe("splitCommand ", () => {
  const inputToExpected = [
    ["cat", ["cat"]],
    [["cat"], ["cat"]],
    [
      ["cat", "hello all my homies"],
      ["cat", "hello all my homies"],
    ],
    ["cat hello world", ["cat", "hello", "world"]],
    ["cat hello 'big world'", ["cat", "hello", "big world"]],
    [`cat hello "big world"`, ["cat", "hello", "big world"]],
    [
      `cat hello "big world's are the greatest"`,
      ["cat", "hello", "big world's are the greatest"],
    ],
    // Too many spaces
    ["cat      ", ["cat"]],
    [["cat   "], ["cat   "]],
    [
      ["cat   ", "hello all my homies    "],
      ["cat   ", "hello all my homies    "],
    ],
    ["cat        hello   world ", ["cat", "hello", "world"]],
    [
      "        cat      hello     'big  world'       ",
      ["cat", "hello", "big  world"],
    ],
    [
      `     cat     hello    "big   world"       `,
      ["cat", "hello", "big   world"],
    ],
  ]

  for (const [input, expectValue] of inputToExpected) {
    test(`should return ${expectValue} for ${input}`, () => {
      expect(splitCommand(input as any)).toEqual(expectValue)
    })
  }
})
