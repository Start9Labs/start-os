import {
  UnionSelectKey,
  unionSelectKey,
  UnionValueKey,
  unionValueKey,
} from "../config/configTypes"
import { ConfigSpec, matchConfigSpec } from "./output"
import * as _I from "../index"
import { camelCase } from "../../scripts/oldSpecToBuilder"
import { deepMerge } from "../util/deepMerge"

export type IfEquals<T, U, Y = unknown, N = never> =
  (<G>() => G extends T ? 1 : 2) extends <G>() => G extends U ? 1 : 2 ? Y : N
export function testOutput<A, B>(): (c: IfEquals<A, B>) => null {
  return () => null
}

/// Testing the types of the input spec
testOutput<ConfigSpec["rpc"]["enable"], boolean>()(null)
testOutput<ConfigSpec["rpc"]["username"], string>()(null)
testOutput<ConfigSpec["rpc"]["username"], string>()(null)

testOutput<ConfigSpec["rpc"]["advanced"]["auth"], string[]>()(null)
testOutput<
  ConfigSpec["rpc"]["advanced"]["serialversion"],
  "segwit" | "non-segwit"
>()(null)
testOutput<ConfigSpec["rpc"]["advanced"]["servertimeout"], number>()(null)
testOutput<
  ConfigSpec["advanced"]["peers"]["addnode"][0]["hostname"],
  string | null | undefined
>()(null)
testOutput<
  ConfigSpec["testListUnion"][0]["union"][UnionValueKey]["name"],
  string
>()(null)
testOutput<ConfigSpec["testListUnion"][0]["union"][UnionSelectKey], "lnd">()(
  null,
)
testOutput<ConfigSpec["mediasources"], Array<"filebrowser" | "nextcloud">>()(
  null,
)

// @ts-expect-error Because enable should be a boolean
testOutput<ConfigSpec["rpc"]["enable"], string>()(null)
// prettier-ignore
// @ts-expect-error Expect that the string is the one above
testOutput<ConfigSpec["testListUnion"][0][UnionSelectKey][UnionSelectKey], "unionSelectKey">()(null);

/// Here we test the output of the matchConfigSpec function
describe("Inputs", () => {
  const validInput: ConfigSpec = {
    mediasources: ["filebrowser"],
    testListUnion: [
      {
        union: { [unionSelectKey]: "lnd", [unionValueKey]: { name: "string" } },
      },
    ],
    rpc: {
      enable: true,
      bio: "This is a bio",
      username: "test",
      password: "test",
      advanced: {
        auth: ["test"],
        serialversion: "segwit",
        servertimeout: 6,
        threads: 3,
        workqueue: 9,
      },
    },
    "zmq-enabled": false,
    txindex: false,
    wallet: { enable: false, avoidpartialspends: false, discardfee: 0.0001 },
    advanced: {
      mempool: {
        maxmempool: 1,
        persistmempool: true,
        mempoolexpiry: 23,
        mempoolfullrbf: true,
      },
      peers: {
        listen: true,
        onlyconnect: true,
        onlyonion: true,
        addnode: [
          {
            hostname: "test",
            port: 1,
          },
        ],
      },
      dbcache: 5,
      pruning: {
        unionSelectKey: "disabled",
        unionValueKey: {},
      },
      blockfilters: {
        blockfilterindex: false,
        peerblockfilters: false,
      },
      bloomfilters: { peerbloomfilters: false },
    },
  }

  test("test valid input", () => {
    const output = matchConfigSpec.unsafeCast(validInput)
    expect(output).toEqual(validInput)
  })
  test("test no longer care about the conversion of min/max and validating", () => {
    matchConfigSpec.unsafeCast(
      deepMerge({}, validInput, { rpc: { advanced: { threads: 0 } } }),
    )
  })
  test("test errors should throw for number in string", () => {
    expect(() =>
      matchConfigSpec.unsafeCast(
        deepMerge({}, validInput, { rpc: { enable: 2 } }),
      ),
    ).toThrowError()
  })
  test("Test that we set serialversion to something not segwit or non-segwit", () => {
    expect(() =>
      matchConfigSpec.unsafeCast(
        deepMerge({}, validInput, {
          rpc: { advanced: { serialversion: "testing" } },
        }),
      ),
    ).toThrowError()
  })
})

describe("camelCase", () => {
  test("'EquipmentClass name'", () => {
    expect(camelCase("EquipmentClass name")).toEqual("equipmentClassName")
  })
  test("'Equipment className'", () => {
    expect(camelCase("Equipment className")).toEqual("equipmentClassName")
  })
  test("'equipment class name'", () => {
    expect(camelCase("equipment class name")).toEqual("equipmentClassName")
  })
  test("'Equipment Class Name'", () => {
    expect(camelCase("Equipment Class Name")).toEqual("equipmentClassName")
  })
  test("'hyphen-name-format'", () => {
    expect(camelCase("hyphen-name-format")).toEqual("hyphenNameFormat")
  })
  test("'underscore_name_format'", () => {
    expect(camelCase("underscore_name_format")).toEqual("underscoreNameFormat")
  })
})
