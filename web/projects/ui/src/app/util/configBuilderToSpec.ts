import { ISB } from '@start9labs/start-sdk'

export async function configBuilderToSpec(
  builder:
    | ISB.InputSpec<Record<string, unknown>, unknown>
    | ISB.InputSpec<Record<string, unknown>, never>,
) {
  return builder.build({} as any)
}
