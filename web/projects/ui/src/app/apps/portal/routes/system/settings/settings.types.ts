import { CB } from '@start9labs/start-sdk'

export interface SettingBtn {
  title: string
  description: string
  icon: string
  action?: Function
  routerLink?: string
}

export const passwordSpec = CB.Config.of({
  currentPassword: CB.Value.text({
    name: 'Current Password',
    required: {
      default: null,
    },
    masked: true,
  }),
  newPassword1: CB.Value.text({
    name: 'New Password',
    required: {
      default: null,
    },
    masked: true,
  }),
  newPassword2: CB.Value.text({
    name: 'Retype New Password',
    required: {
      default: null,
    },
    masked: true,
  }),
})

export type PasswordSpec = typeof passwordSpec.validator._TYPE
