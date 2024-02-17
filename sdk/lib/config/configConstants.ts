import { SmtpValue } from "../types"
import { email } from "../util/patterns"
import { Config, ConfigSpecOf } from "./builder/config"
import { Value } from "./builder/value"
import { Variants } from "./builder/variants"

/**
 * Base SMTP settings, to be used by StartOS for system wide SMTP
 */
export const customSmtp = Config.of<ConfigSpecOf<SmtpValue>, never>({
  server: Value.text({
    name: "SMTP Server",
    required: {
      default: null,
    },
  }),
  port: Value.number({
    name: "Port",
    required: { default: 587 },
    min: 1,
    max: 65535,
    integer: true,
  }),
  from: Value.text({
    name: "From Address",
    required: {
      default: null,
    },
    placeholder: "<name>test@example.com",
    inputmode: "email",
    patterns: [email],
  }),
  login: Value.text({
    name: "Login",
    required: {
      default: null,
    },
  }),
  password: Value.text({
    name: "Password",
    required: false,
    masked: true,
  }),
})

/**
 * For service config. Gives users 3 options for SMTP: (1) disabled, (2) use system SMTP settings, (3) use custom SMTP settings
 */
export const smtpConfig = Value.filteredUnion(
  async ({ effects, utils }) => {
    const smtp = await utils.getSystemSmtp().once()
    return smtp ? [] : ["system"]
  },
  {
    name: "SMTP",
    description: "Optionally provide an SMTP server for sending emails",
    required: { default: "disabled" },
  },
  Variants.of({
    disabled: { name: "Disabled", spec: Config.of({}) },
    system: {
      name: "System Credentials",
      spec: Config.of({
        customFrom: Value.text({
          name: "Custom From Address",
          description:
            "A custom from address for this service. If not provided, the system from address will be used.",
          required: false,
          placeholder: "<name>test@example.com",
          inputmode: "email",
          patterns: [email],
        }),
      }),
    },
    custom: {
      name: "Custom Credentials",
      spec: customSmtp,
    },
  }),
)
