import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'

const SMTP_SPEC: InputSpec = {
  host: {
    type: 'text',
    minLength: null,
    maxLength: null,
    patterns: [],
    inputmode: 'url',
    name: 'Host',
    description: 'Hostname of the SMTP server',
    placeholder: 'e.g. smtp.mailgun.org',
    required: true,
    masked: false,
    warning: null,
    default: null,
    disabled: false,
    immutable: false,
    generate: null,
  },
  port: {
    type: 'number',
    name: 'Port',
    description: 'Port of the SMTP server',
    warning: null,
    placeholder: 'e.g. 587',
    required: true,
    min: 0,
    max: null,
    step: null,
    units: null,
    integer: true,
    default: null,
    disabled: false,
    immutable: false,
  },
  from: {
    type: 'text',
    minLength: null,
    maxLength: null,
    patterns: [],
    inputmode: 'text',
    name: 'From Address',
    description: 'The address that will send the emails',
    placeholder: 'First  Last <email@example.com>',
    required: true,
    masked: false,
    warning: null,
    default: null,
    disabled: false,
    immutable: false,
    generate: null,
  },
  login: {
    type: 'text',
    minLength: null,
    maxLength: null,
    patterns: [],
    inputmode: 'text',
    name: 'Login',
    description: 'Login username for SMTP server',
    required: true,
    masked: false,
    warning: null,
    placeholder: null,
    default: null,
    disabled: false,
    immutable: false,
    generate: null,
  },
  password: {
    type: 'text',
    minLength: null,
    maxLength: null,
    patterns: [],
    inputmode: 'text',
    name: 'Password',
    description: 'Password username for SMTP server',
    required: true,
    masked: true,
    warning: null,
    placeholder: null,
    default: null,
    disabled: false,
    immutable: false,
    generate: null,
  },
  tls: {
    type: 'toggle',
    name: 'Enable TLS',
    description: 'Whether or not to enable TLS certificate security checks',
    warning: null,
    default: true,
    disabled: false,
    immutable: false,
  },
}

export const EMAIL_SPEC: InputSpec = {
  enabled: {
    type: 'toggle',
    name: 'Enable Email Notifications',
    description:
      'Whether or not to receive email notifications from your Embassy',
    warning: null,
    default: false,
    disabled: false,
    immutable: false,
  },
  address: {
    type: 'text',
    minLength: null,
    maxLength: null,
    patterns: [],
    inputmode: 'email',
    name: 'Receive Address',
    description: 'The address you want to receive email notifications',
    warning: null,
    placeholder: 'e.g. you@protonmail.com',
    required: true,
    masked: false,
    default: null,
    disabled: false,
    immutable: false,
    generate: null,
  },
  smtp: {
    type: 'object',
    name: 'SMTP Settings',
    description: 'Settings and credentials for your chosen SMTP server',
    warning: null,
    spec: SMTP_SPEC,
  },
}
