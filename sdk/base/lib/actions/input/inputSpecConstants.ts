import { SmtpValue } from '../../types'
import { GetSystemSmtp, Patterns } from '../../util'
import { InputSpec, InputSpecOf } from './builder/inputSpec'
import { Value } from './builder/value'
import { Variants } from './builder/variants'

/**
 * Creates an SMTP field spec with provider-specific defaults pre-filled.
 */
function smtpFields(
  defaults: {
    host?: string
    port?: number
    security?: 'starttls' | 'tls'
  } = {},
): InputSpec<SmtpValue> {
  return InputSpec.of<InputSpecOf<SmtpValue>>({
    host: Value.text({
      name: 'Host',
      required: true,
      default: defaults.host ?? null,
      placeholder: 'smtp.example.com',
    }),
    port: Value.number({
      name: 'Port',
      required: true,
      default: defaults.port ?? 587,
      min: 1,
      max: 65535,
      integer: true,
    }),
    security: Value.select({
      name: 'Connection Security',
      default: defaults.security ?? 'starttls',
      values: {
        starttls: 'STARTTLS',
        tls: 'TLS',
      },
    }),
    from: Value.text({
      name: 'From Address',
      required: true,
      default: null,
      placeholder: 'Example Name <test@example.com>',
      patterns: [Patterns.emailWithName],
    }),
    username: Value.text({
      name: 'Username',
      required: true,
      default: null,
    }),
    password: Value.text({
      name: 'Password',
      required: false,
      default: null,
      masked: true,
    }),
  })
}

/**
 * Base SMTP settings with no provider-specific defaults.
 */
export const customSmtp = smtpFields()

/**
 * Provider presets for SMTP configuration.
 * Each variant has SMTP fields pre-filled with the provider's recommended settings.
 */
export const smtpProviderVariants = Variants.of({
  gmail: {
    name: 'Gmail',
    spec: smtpFields({
      host: 'smtp.gmail.com',
      port: 587,
      security: 'starttls',
    }),
  },
  ses: {
    name: 'Amazon SES',
    spec: smtpFields({
      host: 'email-smtp.us-east-1.amazonaws.com',
      port: 587,
      security: 'starttls',
    }),
  },
  sendgrid: {
    name: 'SendGrid',
    spec: smtpFields({
      host: 'smtp.sendgrid.net',
      port: 587,
      security: 'starttls',
    }),
  },
  mailgun: {
    name: 'Mailgun',
    spec: smtpFields({
      host: 'smtp.mailgun.org',
      port: 587,
      security: 'starttls',
    }),
  },
  protonmail: {
    name: 'Proton Mail',
    spec: smtpFields({
      host: 'smtp.protonmail.ch',
      port: 587,
      security: 'starttls',
    }),
  },
  other: {
    name: 'Other',
    spec: customSmtp,
  },
})

/**
 * System SMTP settings with provider presets.
 * Wraps smtpProviderVariants in a union for use by the system email settings page.
 */
export const systemSmtpSpec = InputSpec.of({
  provider: Value.union({
    name: 'Provider',
    default: null as any,
    variants: smtpProviderVariants,
  }),
})

const smtpVariants = Variants.of({
  disabled: { name: 'Disabled', spec: InputSpec.of({}) },
  system: {
    name: 'System Credentials',
    spec: InputSpec.of({
      customFrom: Value.text({
        name: 'Custom From Address',
        description:
          'A custom from address for this service. If not provided, the system from address will be used.',
        required: false,
        default: null,
        placeholder: 'Name <test@example.com>',
        patterns: [Patterns.emailWithName],
      }),
    }),
  },
  custom: {
    name: 'Custom Credentials',
    spec: InputSpec.of({
      provider: Value.union({
        name: 'Provider',
        default: null as any,
        variants: smtpProviderVariants,
      }),
    }),
  },
})
/**
 * For service inputSpec. Gives users 3 options for SMTP: (1) disabled, (2) use system SMTP settings, (3) use custom SMTP settings with provider presets
 */
export const smtpInputSpec = Value.dynamicUnion(async ({ effects }) => {
  const smtp = await new GetSystemSmtp(effects).once()
  const disabled = smtp ? [] : ['system']
  return {
    name: 'SMTP',
    description: 'Optionally provide an SMTP server for sending emails',
    default: 'disabled',
    disabled,
    variants: smtpVariants,
  }
}, smtpVariants.validator)
