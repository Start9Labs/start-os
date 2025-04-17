import { inject, Injectable, TemplateRef } from '@angular/core'
import {
  TuiResponsiveDialogOptions,
  TuiResponsiveDialogService,
} from '@taiga-ui/addon-mobile'
import { TuiAlertOptions } from '@taiga-ui/core'
import { TUI_CONFIRM, TuiConfirmData } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PROMPT, PromptOptions } from '../components/prompt.component'
import { i18nPipe } from '../i18n/i18n.pipe'
import { i18nKey } from '../i18n/i18n.providers'

@Injectable({
  providedIn: 'root',
})
export class DialogService {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly i18n = inject(i18nPipe)

  openPrompt<T = void>(
    options: Partial<TuiResponsiveDialogOptions<PromptOptions>> & {
      label: i18nKey
      data: PromptOptions
    },
  ) {
    const { message, label, placeholder, buttonText } = options.data

    return this.dialogs.open<T>(PROMPT, {
      label: this.i18n.transform(options.label),
      data: {
        ...options.data,
        message: this.i18n.transform(message),
        label: this.i18n.transform(label),
        placeholder: this.i18n.transform(placeholder),
        buttonText: this.i18n.transform(buttonText),
      },
    })
  }

  openConfirm<T = void>(
    options: Partial<TuiResponsiveDialogOptions<TuiConfirmData>> & {
      label: i18nKey
      data?: TuiConfirmData & {
        content?: PolymorpheusComponent<any> | i18nKey
        yes?: i18nKey
        no?: i18nKey
      }
    },
  ) {
    options.data = options.data || {}
    const { content, yes, no } = options.data

    return this.dialogs.open<T>(TUI_CONFIRM, {
      label: this.i18n.transform(options.label),
      data: {
        ...options.data,
        content: isI18n(content) ? this.i18n.transform(content) : content,
        yes: this.i18n.transform(yes),
        no: this.i18n.transform(no),
      },
    })
  }

  openAlert<T = void>(
    message: i18nKey | undefined,
    options: Partial<TuiAlertOptions<any>> & {
      label?: i18nKey
    } = {},
  ) {
    return this.dialogs.open<T>(this.i18n.transform(message), {
      ...options,
      label: this.i18n.transform(options.label),
    })
  }

  openComponent<T = void>(
    component: PolymorpheusComponent<any> | TemplateRef<any>,
    options: Partial<TuiResponsiveDialogOptions<any>> & {
      label?: i18nKey
    } = {},
  ) {
    return this.dialogs.open<T>(component, {
      ...options,
      label: this.i18n.transform(options.label),
    })
  }
}

function isI18n(
  content: PolymorpheusComponent<any> | i18nKey | undefined,
): content is i18nKey {
  return typeof content === 'string'
}
