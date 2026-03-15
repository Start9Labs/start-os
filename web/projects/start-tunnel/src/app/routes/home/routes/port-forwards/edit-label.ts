import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { ErrorService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiTextfield,
} from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'

export interface EditLabelData {
  readonly source: string
  readonly label: T.Tunnel.PortForwardEntry['label']
}

@Component({
  template: `
    <form tuiForm [formGroup]="form">
      <tui-textfield>
        <label tuiLabel>Label</label>
        <input tuiTextfield formControlName="label" />
      </tui-textfield>
      <tui-error formControlName="label" />
      <footer>
        <button tuiButton [disabled]="form.invalid" (click)="onSave()">
          Save
        </button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ReactiveFormsModule, TuiButton, TuiError, TuiTextfield, TuiForm],
})
export class PortForwardsEditLabel {
  private readonly api = inject(ApiService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)

  protected readonly context =
    injectContext<TuiDialogContext<void, EditLabelData>>()

  protected readonly form = inject(NonNullableFormBuilder).group({
    label: [this.context.data.label, Validators.required],
  })

  protected async onSave() {
    const loader = this.loading.open('').subscribe()

    try {
      await this.api.updateForwardLabel({
        source: this.context.data.source,
        label: this.form.getRawValue().label,
      })
    } catch (e: any) {
      console.error(e)
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
      this.context.$implicit.complete()
    }
  }
}

export const PORT_FORWARDS_EDIT_LABEL = new PolymorpheusComponent(
  PortForwardsEditLabel,
)
