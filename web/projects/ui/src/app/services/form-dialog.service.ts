import { inject, Injectable, Injector, Type } from '@angular/core'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TuiConfirmData, TuiConfirmService } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'

const PROMPT: Partial<TuiDialogOptions<TuiConfirmData>> = {
  label: 'Unsaved Changes',
  data: {
    content: 'You have unsaved changes. Are you sure you want to leave?',
    yes: 'Leave',
    no: 'Cancel',
  },
}

@Injectable({ providedIn: 'root' })
export class FormDialogService {
  private readonly dialogs = inject(TuiDialogService)
  private readonly formService = new TuiConfirmService()
  private readonly prompt = this.formService.withConfirm(PROMPT)
  private readonly injector = Injector.create({
    parent: inject(Injector),
    providers: [
      {
        provide: TuiConfirmService,
        useValue: this.formService,
      },
    ],
  })

  open<T>(component: Type<any>, options: Partial<TuiDialogOptions<T>> = {}) {
    this.dialogs
      .open(new PolymorpheusComponent(component, this.injector), {
        closeable: this.prompt,
        dismissible: this.prompt,
        ...options,
      })
      .subscribe({
        complete: () => this.formService.markAsPristine(),
      })
  }
}
