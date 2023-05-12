import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { customSmtp } from '@start9labs/start-sdk/lib/config/configConstants'

export const emailSpec = Config.of({
  enabled: Value.toggle({
    name: 'Enable Email Notifications',
    description:
      'Whether or not to receive email notifications from your Embassy',
    default: false,
  }),
  address: Value.text({
    name: 'Receive Address',
    description: 'The address you want to receive email notifications',
    placeholder: 'e.g. you@protonmail.com',
    inputmode: 'email',
    required: { default: null },
  }),
  smtp: Value.object(
    {
      name: 'SMTP Credentials',
    },
    customSmtp,
  ),
})

export type EmailSpec = typeof emailSpec.validator._TYPE
