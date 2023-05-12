import { Config } from '@start9labs/start-sdk/lib/config/builder/config'

export async function configBuilderToSpec(
  builder:
    | Config<Record<string, unknown>, unknown, unknown>
    | Config<Record<string, unknown>, never, never>,
) {
  return builder.build({} as any)
}
