import {
  AbstractControl,
  NonNullableFormBuilder,
  ValidationErrors,
  Validators,
} from '@angular/forms'
import { FormRawValue } from 'src/app/services/form.service'

export const OUTBOUND_VALIDATION_ERRORS = {
  required: 'Required',
  invalidExtension: 'Please upload a .conf file',
}

export interface OutboundVpn {
  id: string // Interface name (e.g., 'wg_proton')
  label: string
  target: string // 'Internet' or another VPN label
  enabled: boolean
  // WireGuard config
  privateKey: string
  addresses: string[]
  dns: string[]
  // Peer config
  publicKey: string
  endpoint: string
  allowedIps: string[]
  persistentKeepalive: number
}

export interface OutboundVpnTableItem {
  id: string
  label: string
  target: string
  enabled: boolean
  usedBy: string // Stub for now - will come from security profiles later
}

function fileExtensionValidator(
  allowedExtensions: string[],
): (control: AbstractControl) => ValidationErrors | null {
  return (control: AbstractControl): ValidationErrors | null => {
    const file = control.value as File | null
    if (!file) return null

    const extension = file.name.split('.').pop()?.toLowerCase()
    if (!extension || !allowedExtensions.includes(extension)) {
      return { invalidExtension: { allowed: allowedExtensions.join(', ') } }
    }
    return null
  }
}

export function getOutboundVpnForm(builder: NonNullableFormBuilder) {
  return builder.group({
    label: builder.control('', [Validators.required]),
    target: builder.control('Internet', [Validators.required]),
  })
}

export function getAddOutboundVpnForm(builder: NonNullableFormBuilder) {
  return builder.group({
    label: builder.control('', [Validators.required]),
    config: builder.control<File | null>(null, [
      Validators.required,
      fileExtensionValidator(['conf']),
    ]),
    target: builder.control('Internet', [Validators.required]),
  })
}

export type OutboundVpnForm = FormRawValue<
  ReturnType<typeof getOutboundVpnForm>
>
export type AddOutboundVpnForm = FormRawValue<
  ReturnType<typeof getAddOutboundVpnForm>
>

/**
 * Build the full connection path for a VPN
 * e.g., ['Mullvad', 'Proton', 'Internet']
 */
export function buildConnectionPath(
  vpnLabel: string,
  allVpns: OutboundVpnTableItem[],
): string[] {
  const path: string[] = [vpnLabel]
  let current = allVpns.find(v => v.label === vpnLabel)

  // Follow the chain until we hit Internet or a cycle
  const visited = new Set<string>([vpnLabel])
  while (current && current.target !== 'Internet') {
    if (visited.has(current.target)) {
      // Cycle detected - shouldn't happen but handle gracefully
      path.push('...')
      break
    }
    visited.add(current.target)
    path.push(current.target)
    current = allVpns.find(v => v.label === current!.target)
  }

  path.push('Internet')
  return path
}
