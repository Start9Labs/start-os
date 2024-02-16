import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiPromptData } from '@taiga-ui/kit'
import { NetworkInfo } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'

export const REMOVE: Partial<TuiDialogOptions<TuiPromptData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Remove clearnet address?',
    yes: 'Remove',
    no: 'Cancel',
  },
}

export function getClearnetSpec({
  domains,
  start9ToSubdomain,
}: NetworkInfo): Promise<InputSpec> {
  const start9ToDomain = `${start9ToSubdomain?.value}.start9.to`
  const base = start9ToSubdomain ? { [start9ToDomain]: start9ToDomain } : {}

  const values = domains.reduce((prev, curr) => {
    return {
      [curr.value]: curr.value,
      ...prev,
    }
  }, base)

  return configBuilderToSpec(
    Config.of({
      domain: Value.select({
        name: 'Domain',
        required: { default: null },
        values,
      }),
      subdomain: Value.text({
        name: 'Subdomain',
        required: false,
      }),
    }),
  )
}
