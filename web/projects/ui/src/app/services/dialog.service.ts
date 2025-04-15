import { inject, Injectable } from '@angular/core'
import {
  TuiResponsiveDialogOptions,
  TuiResponsiveDialogService,
} from '@taiga-ui/addon-mobile'
import { PROMPT } from '../routes/portal/modals/prompt.component'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TUI_CONFIRM } from '@taiga-ui/kit'

@Injectable({
  providedIn: 'root',
})
export class DialogService {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly i18n = inject(i18nPipe)

  openPrompt<T = void>(options: {
    label: i18nKey
    data: Partial<{
      label: i18nKey
      message: i18nKey
      placeholder: i18nKey
      buttonText: i18nKey
      required: boolean
      initialValue: string
    }>
  }) {
    const { message, label, placeholder, buttonText } = options.data

    return this.dialogs.open<T>(PROMPT, {
      label: this.i18n.transform(options.label),
      data: {
        ...options.data,
        message: message ? this.i18n.transform(message) : undefined,
        label: label ? this.i18n.transform(label) : undefined,
        placeholder: placeholder ? this.i18n.transform(placeholder) : undefined,
        buttonText: buttonText ? this.i18n.transform(buttonText) : undefined,
      },
    })
  }

  openConfirm<T = void>(options: {
    label: i18nKey
    data: Partial<{
      content: any
      yes: i18nKey
      no: i18nKey
    }>
  }) {
    const { yes, no } = options.data

    return this.dialogs.open<T>(TUI_CONFIRM, {
      label: this.i18n.transform(options.label),
      data: {
        ...options.data,
        yes: yes ? this.i18n.transform(yes) : undefined,
        no: no ? this.i18n.transform(no) : undefined,
      },
    })
  }

  openAlert<T = void>(
    message: i18nKey,
    options: Partial<TuiResponsiveDialogOptions<any>> & {
      label?: i18nKey
    } = {},
  ) {
    return this.dialogs.open<T>(this.i18n.transform(message), {
      ...options,
      label: options.label ? this.i18n.transform(options.label) : undefined,
    })
  }
}
