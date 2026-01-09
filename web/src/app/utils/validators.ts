import { AbstractControl, ValidationErrors, ValidatorFn } from '@angular/forms'

export class CustomValidators {
  static ipv4(): ValidatorFn {
    return (control: AbstractControl): ValidationErrors | null => {
      if (!control.value) return null

      const ipv4Regex = /^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/
      const match = control.value.match(ipv4Regex)

      if (!match) return { ipv4: true }

      // Validate each octet is 0-255
      const octets = [match[1], match[2], match[3], match[4]]
      const valid = octets.every(octet => {
        const num = parseInt(octet, 10)
        return num >= 0 && num <= 255
      })

      return valid ? null : { ipv4: true }
    }
  }

  static ipv6(): ValidatorFn {
    return (control: AbstractControl): ValidationErrors | null => {
      if (!control.value) return null

      // Simplified IPv6 validation (full validation is complex)
      const ipv6Regex =
        /^(([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:))$/

      return ipv6Regex.test(control.value) ? null : { ipv6: true }
    }
  }

  static ipv4OrIpv6(): ValidatorFn {
    return (control: AbstractControl): ValidationErrors | null => {
      if (!control.value) return null

      const ipv4Result = CustomValidators.ipv4()(control)
      const ipv6Result = CustomValidators.ipv6()(control)

      // Valid if either IPv4 or IPv6 is valid
      return ipv4Result === null || ipv6Result === null ? null : { ip: true }
    }
  }
}
