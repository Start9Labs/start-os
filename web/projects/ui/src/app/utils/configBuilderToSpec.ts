import { ISB } from '@start9labs/start-sdk'

export async function configBuilderToSpec(
  builder: ISB.InputSpec<Record<string, unknown>>,
) {
  return builder.build({} as any)
}
