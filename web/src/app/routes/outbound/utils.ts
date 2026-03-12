import {
  AbstractControl,
  NonNullableFormBuilder,
  ValidationErrors,
  Validators,
} from '@angular/forms'
import { FormRawValue } from 'src/app/services/form.service'
import { OutboundVpn } from 'src/app/services/api/api.service'
import { CustomValidators } from 'src/app/utils/validators'

export const OUTBOUND_VALIDATION_ERRORS = {
  required: 'Required',
  invalidExtension: 'Please upload a .conf file',
  duplicateName: 'A VPN with this label already exists',
  interfaceNameLength: 'Label is too long (max ~12 characters)',
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

const labelValidators = (existingLabels: string[]) => [
  Validators.required,
  CustomValidators.duplicateName(existingLabels),
  CustomValidators.interfaceNameLength('wg_', 15),
]

export function getOutboundVpnForm(
  builder: NonNullableFormBuilder,
  existingLabels: string[],
) {
  return builder.group({
    label: builder.control('', labelValidators(existingLabels)),
    target: builder.control('Internet', [Validators.required]),
  })
}

export function getAddOutboundVpnForm(
  builder: NonNullableFormBuilder,
  existingLabels: string[],
) {
  return builder.group({
    label: builder.control('', labelValidators(existingLabels)),
    config: builder.control<File | null>(null, [
      Validators.required,
      fileExtensionValidator(['conf']),
    ]),
    target: builder.control('Internet', [Validators.required]),
  })
}

export interface AddClientDialogData {
  targets: string[]
  existingLabels: string[]
}

export type OutboundVpnForm = FormRawValue<
  ReturnType<typeof getOutboundVpnForm>
>
export type AddOutboundVpnForm = FormRawValue<
  ReturnType<typeof getAddOutboundVpnForm>
>

/**
 * Compute which VPN labels are safe targets for a given VPN.
 * A target T is unsafe if following T's chain eventually reaches selfLabel (cycle).
 */
export function getSafeTargets(
  selfLabel: string,
  allVpns: OutboundVpn[],
): string[] {
  return [
    'Internet',
    ...allVpns
      .filter(v => v.label !== selfLabel)
      .filter(v => {
        const visited = new Set<string>([selfLabel])
        let current: OutboundVpn | undefined = v
        while (current && current.target !== 'Internet') {
          if (visited.has(current.target)) return false
          visited.add(current.target)
          current = allVpns.find(x => x.label === current!.target)
        }
        return true
      })
      .map(v => v.label),
  ]
}

/**
 * Build the full connection path for a VPN
 * e.g., ['Mullvad', 'Proton', 'Internet']
 */
export function buildConnectionPath(
  vpnLabel: string,
  allVpns: OutboundVpn[],
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
