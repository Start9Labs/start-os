import { ISB } from '@start9labs/start-sdk'

export async function configBuilderToSpec(builder: ISB.InputSpec<any>) {
  return builder.build({} as any).then(a => a.spec)
}
