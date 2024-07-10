import { ListValueSpecOf, isValueSpecListOf } from "../config/configTypes"
import { Config } from "../config/builder/config"
import { List } from "../config/builder/list"
import { Value } from "../config/builder/value"

describe("Config Types", () => {
  test("isValueSpecListOf", async () => {
    const options = [List.obj, List.text]
    for (const option of options) {
      const test = (option as any)(
        {} as any,
        { spec: Config.of({}) } as any,
      ) as any
      const someList = await Value.list(test).build({} as any)
      if (isValueSpecListOf(someList, "text")) {
        someList.spec satisfies ListValueSpecOf<"text">
      } else if (isValueSpecListOf(someList, "object")) {
        someList.spec satisfies ListValueSpecOf<"object">
      } else {
        throw new Error(
          "Failed to figure out the type: " + JSON.stringify(someList),
        )
      }
    }
  })
})
