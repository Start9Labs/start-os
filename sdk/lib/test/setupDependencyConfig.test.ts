import { sdk } from "./output.sdk"

describe("setupDependencyInputSpec", () => {
  test("test", () => {
    const testInputSpec = sdk.InputSpec.of({
      test: sdk.Value.text({
        name: "testValue",
        required: false,
      }),
    })

    const testInputSpec2 = sdk.InputSpec.of({
      test2: sdk.Value.text({
        name: "testValue2",
        required: false,
      }),
    })
    const remoteTest = sdk.DependencyInputSpec.of({
      localInputSpecSpec: testInputSpec,
      remoteInputSpecSpec: testInputSpec2,
      dependencyInputSpec: async ({}) => {},
    })
    sdk.setupDependencyInputSpec(testInputSpec, {
      "remote-test": remoteTest,
    })
  })
})
