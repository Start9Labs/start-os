import { ListValueSpecOf, isValueSpecListOf } from "../input/inputSpecTypes"
import { InputSpec } from "../input/builder/inputSpec"
import { List } from "../input/builder/list"
import { Value } from "../input/builder/value"

describe("InputSpec Types", () => {
  test("isValueSpecListOf", async () => {
    const options = [List.obj, List.text]
    for (const option of options) {
      const test = (option as any)(
        {} as any,
        { spec: InputSpec.of({}) } as any,
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
