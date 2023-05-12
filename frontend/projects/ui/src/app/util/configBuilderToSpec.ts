import { Config } from '@start9labs/start-sdk/lib/config/builder/config'

export async function configBuilderToSpec(
  builder: Config<{}, unknown, unknown>,
) {
  return builder.build({} as any)
}
