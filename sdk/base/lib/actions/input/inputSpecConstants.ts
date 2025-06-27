import { SmtpValue } from "../../types"
import { GetSystemSmtp, Patterns } from "../../util"
import { InputSpec, InputSpecOf } from "./builder/inputSpec"
import { Value } from "./builder/value"
import { Variants } from "./builder/variants"

/**
 * Base SMTP settings, to be used by StartOS for system wide SMTP
 */
export const customSmtp: InputSpec<SmtpValue> = InputSpec.of<
  InputSpecOf<SmtpValue>
>({
  server: Value.text({
    name: "SMTP Server",
    required: true,
    default: null,
  }),
  port: Value.number({
    name: "Port",
    required: true,
    default: 587,
    min: 1,
    max: 65535,
    integer: true,
  }),
  from: Value.text({
    name: "From Address",
    required: true,
    default: null,
    placeholder: "Example Name <test@example.com>",
    inputmode: "email",
    patterns: [Patterns.emailWithName],
  }),
  login: Value.text({
    name: "Login",
    required: true,
    default: null,
  }),
  password: Value.text({
    name: "Password",
    required: false,
    default: null,
    masked: true,
  }),
})

const smtpVariants = Variants.of({
  disabled: { name: "Disabled", spec: InputSpec.of({}) },
  system: {
    name: "System Credentials",
    spec: InputSpec.of({
      customFrom: Value.text({
        name: "Custom From Address",
        description:
          "A custom from address for this service. If not provided, the system from address will be used.",
        required: false,
        default: null,
        placeholder: "<name>test@example.com",
        inputmode: "email",
        patterns: [Patterns.email],
      }),
    }),
  },
  custom: {
    name: "Custom Credentials",
    spec: customSmtp,
  },
})
/**
 * For service inputSpec. Gives users 3 options for SMTP: (1) disabled, (2) use system SMTP settings, (3) use custom SMTP settings
 */
export const smtpInputSpec = Value.dynamicUnion(async ({ effects }) => {
  const smtp = await new GetSystemSmtp(effects).once()
  const disabled = smtp ? [] : ["system"]
  return {
    name: "SMTP",
    description: "Optionally provide an SMTP server for sending emails",
    default: "disabled",
    disabled,
    variants: smtpVariants,
  }
}, smtpVariants)
