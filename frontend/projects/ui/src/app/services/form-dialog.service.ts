import { inject, Injectable, Injector, Type } from '@angular/core'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TuiDialogFormService, TuiPromptData } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'

export const PROMPT: Partial<TuiDialogOptions<TuiPromptData>> = {
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
  private readonly formService = new TuiDialogFormService(this.dialogs)
  private readonly prompt = this.formService.withPrompt(PROMPT)
  private readonly injector = Injector.create({
    parent: inject(Injector),
    providers: [
      {
        provide: TuiDialogFormService,
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
