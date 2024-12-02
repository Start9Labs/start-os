import { ISB } from '@start9labs/start-sdk'

export interface SettingBtn {
  title: string
  description: string
  icon: string
  action?: Function
  routerLink?: string
}

export const passwordSpec = ISB.InputSpec.of({
  currentPassword: ISB.Value.text({
    name: 'Current Password',
    required: true,
    default: null,
    masked: true,
  }),
  newPassword1: ISB.Value.text({
    name: 'New Password',
    required: true,
    default: null,
    masked: true,
  }),
  newPassword2: ISB.Value.text({
    name: 'Retype New Password',
    required: true,
    default: null,
    masked: true,
  }),
})

export type PasswordSpec = typeof passwordSpec.validator._TYPE
