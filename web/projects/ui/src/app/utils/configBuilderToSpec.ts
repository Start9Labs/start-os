import { CB } from '@start9labs/start-sdk'

export async function configBuilderToSpec(
  builder:
    | CB.Config<Record<string, unknown>, unknown>
    | CB.Config<Record<string, unknown>, never>,
) {
  return builder.build({} as any)
}
