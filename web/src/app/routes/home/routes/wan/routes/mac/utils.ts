import { NonNullableFormBuilder, Validators } from '@angular/forms'
import { FormRawValue } from 'src/app/services/form.service'
import { CustomValidators } from 'src/app/utils/validators'

export const MAC_STRATEGIES = ['router', 'custom'] as const

export const MAC_LABELS: Record<MacStrategy | 'mac', string> = {
  router: 'Router',
  custom: 'Custom',
  mac: 'MAC Address',
}

export type MacStrategy = (typeof MAC_STRATEGIES)[number]

export function getMacForm(builder: NonNullableFormBuilder) {
  return builder.group({
    strategy: builder.control<MacStrategy>('router'),
    mac: builder.control('', [CustomValidators.mac()]),
  })
}

export type MacForm = FormRawValue<ReturnType<typeof getMacForm>>

export function updateMacValidators(
  form: ReturnType<typeof getMacForm>,
  strategy: MacStrategy,
): void {
  const { mac } = form.controls

  mac.clearValidators()
  mac.addValidators([CustomValidators.mac()])

  if (strategy === 'custom') {
    mac.addValidators([Validators.required])
  }

  mac.updateValueAndValidity()
}
