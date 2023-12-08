import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'

export interface SettingBtn {
  title: string
  description: string
  icon: string
  action?: Function
  href?: string
  routerLink?: string
}

export const passwordSpec = Config.of({
  currentPassword: Value.text({
    name: 'Current Password',
    required: {
      default: null,
    },
    masked: true,
  }),
  newPassword1: Value.text({
    name: 'New Password',
    required: {
      default: null,
    },
    masked: true,
  }),
  newPassword2: Value.text({
    name: 'Retype New Password',
    required: {
      default: null,
    },
    masked: true,
  }),
})

export type PasswordSpec = typeof passwordSpec.validator._TYPE
