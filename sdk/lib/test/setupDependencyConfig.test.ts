import { sdk } from "./output.sdk"

describe("setupDependencyConfig", () => {
  test("test", () => {
    const testConfig = sdk.Config.of({
      test: sdk.Value.text({
        name: "testValue",
        required: false,
      }),
    })

    const testConfig2 = sdk.Config.of({
      test2: sdk.Value.text({
        name: "testValue2",
        required: false,
      }),
    })
    const remoteTest = sdk.DependencyConfig.of({
      localConfig: testConfig,
      remoteConfig: testConfig2,
      dependencyConfig: async ({}) => {},
    })
    sdk.setupDependencyConfig(testConfig, {
      remoteTest,
    })
  })
})
