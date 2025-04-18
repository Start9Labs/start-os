import { inject, Injectable, Injector, Type } from '@angular/core'
import { DialogService, i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiConfirmData, TuiConfirmService } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'

@Injectable({ providedIn: 'root' })
export class FormDialogService {
  private readonly dialog = inject(DialogService)
  private readonly i18n = inject(i18nPipe)
  private readonly formService = new TuiConfirmService()
  private readonly PROMPT: Partial<TuiDialogOptions<TuiConfirmData>> = {
    label: this.i18n.transform('Unsaved changes'),
    data: {
      content: this.i18n.transform(
        'You have unsaved changes. Are you sure you want to leave?',
      ),
      yes: this.i18n.transform('Leave'),
      no: this.i18n.transform('Cancel'),
    },
  }

  private readonly prompt = this.formService.withConfirm(this.PROMPT)
  private readonly injector = Injector.create({
    parent: inject(Injector),
    providers: [
      {
        provide: TuiConfirmService,
        useValue: this.formService,
      },
    ],
  })

  open<T>(
    component: Type<any>,
    options: Partial<TuiDialogOptions<T>> & {
      label?: i18nKey
    } = {},
  ) {
    this.dialog
      .openComponent(new PolymorpheusComponent(component, this.injector), {
        closeable: this.prompt,
        dismissible: this.prompt,
        ...options,
      })
      .subscribe({
        complete: () => this.formService.markAsPristine(),
      })
  }
}
