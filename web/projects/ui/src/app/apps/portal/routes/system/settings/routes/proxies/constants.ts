import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiPromptData } from '@taiga-ui/kit'

export const DELETE_OPTIONS: Partial<TuiDialogOptions<TuiPromptData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Delete proxy? This action cannot be undone.',
    yes: 'Delete',
    no: 'Cancel',
  },
}

export const wireguardSpec = Config.of({
  name: Value.text({
    name: 'Name',
    description: 'A friendly name to help you remember and identify this proxy',
    required: { default: null },
  }),
  config: Value.file({
    name: 'Wiregaurd Config',
    required: { default: null },
    extensions: ['.conf'],
  }),
})

export type WireguardSpec = typeof wireguardSpec.validator._TYPE
export type ProxyUpdate = Partial<{
  name: string
  primaryInbound: true
  primaryOutbound: true
}>
