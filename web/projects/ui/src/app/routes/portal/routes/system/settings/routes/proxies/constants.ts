import { ISB } from '@start9labs/start-sdk'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiConfirmData } from '@taiga-ui/kit'

export const DELETE_OPTIONS: Partial<TuiDialogOptions<TuiConfirmData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Delete proxy? This action cannot be undone.',
    yes: 'Delete',
    no: 'Cancel',
  },
}

export const wireguardSpec = ISB.InputSpec.of({
  name: ISB.Value.text({
    name: 'Name',
    description: 'A friendly name to help you remember and identify this proxy',
    required: true,
    default: null,
  }),
  // @TODO Matt same here
  // config: ISB.Value.file({
  //   name: 'Wiregaurd Config',
  //   required: { default: null },
  //   extensions: ['.conf'],
  // }),
})

export type WireguardSpec = typeof wireguardSpec.validator._TYPE
export type ProxyUpdate = {
  name: string
}
