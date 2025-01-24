import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnInit,
} from '@angular/core'
import { FormGroup, ReactiveFormsModule } from '@angular/forms'
import { RouterModule } from '@angular/router'
import { IST } from '@start9labs/start-sdk'
import {
  tuiMarkControlAsTouchedAndValidate,
  TuiValueChanges,
} from '@taiga-ui/cdk'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { TuiConfirmService } from '@taiga-ui/kit'
import { POLYMORPHEUS_CONTEXT } from '@taiga-ui/polymorpheus'
import { compare, Operation } from 'fast-json-patch'
import { FormModule } from 'src/app/routes/portal/components/form/form.module'
import { InvalidService } from 'src/app/routes/portal/components/form/invalid.service'
import { FormService } from 'src/app/services/form.service'

export interface ActionButton<T> {
  text: string
  handler?: (value: T) => Promise<boolean | void> | void
  link?: string
}

export interface FormContext<T> {
  spec: IST.InputSpec
  buttons: ActionButton<T>[]
  value?: T
  operations?: Operation[]
}

@Component({
  standalone: true,
  selector: 'app-form',
  template: `
    <form
      [formGroup]="form"
      (submit.capture.prevent)="(0)"
      (reset.capture.prevent.stop)="onReset()"
      (tuiValueChanges)="markAsDirty()"
    >
      <form-group [spec]="spec" />
      <footer>
        <ng-content />
        <ng-container *ngFor="let button of buttons; let last = last">
          <button
            *ngIf="button.handler; else link"
            tuiButton
            [appearance]="last ? 'primary' : 'flat-grayscale'"
            [type]="last ? 'submit' : 'button'"
            (click)="onClick(button.handler)"
          >
            {{ button.text }}
          </button>
          <ng-template #link>
            <a
              tuiButton
              appearance="flat-grayscale"
              [routerLink]="button.link"
              (click)="close()"
            >
              {{ button.text }}
            </a>
          </ng-template>
        </ng-container>
      </footer>
    </form>
  `,
  styles: [
    `
      footer {
        position: sticky;
        bottom: 0;
        z-index: 10;
        display: flex;
        justify-content: flex-end;
        padding: 1rem 0;
        margin: 1rem 0 -1rem;
        gap: 1rem;
        background: var(--tui-background-elevation-1);
        border-top: 1px solid var(--tui-background-base-alt);
      }
    `,
  ],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    RouterModule,
    TuiValueChanges,
    TuiButton,
    FormModule,
  ],
  providers: [InvalidService],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormComponent<T extends Record<string, any>> implements OnInit {
  private readonly confirmService = inject(TuiConfirmService)
  private readonly formService = inject(FormService)
  private readonly invalidService = inject(InvalidService)
  private readonly context = inject<TuiDialogContext<void, FormContext<T>>>(
    POLYMORPHEUS_CONTEXT,
    { optional: true },
  )

  @Input() spec = this.context?.data.spec || {}
  @Input() buttons = this.context?.data.buttons || []
  @Input() operations = this.context?.data.operations || []
  @Input() value?: T = this.context?.data.value

  form = new FormGroup({})

  ngOnInit() {
    this.confirmService.markAsPristine()
    this.form = this.formService.createForm(this.spec, this.value)
    this.process(this.operations)
  }

  onReset() {
    this.form = this.formService.createForm(this.spec)
    tuiMarkControlAsTouchedAndValidate(this.form)
    this.markAsDirty()
  }

  async onClick(handler: Required<ActionButton<T>>['handler']) {
    tuiMarkControlAsTouchedAndValidate(this.form)
    this.invalidService.scrollIntoView()

    if (this.form.valid && (await handler(this.form.value as T))) {
      this.close()
    }
  }

  markAsDirty() {
    this.confirmService.markAsDirty()
  }

  close() {
    this.context?.$implicit.complete()
  }

  private process(operations: Operation[]) {
    operations.forEach(operation => {
      const control = this.form.get(operation.path.substring(1).split('/'))

      if (!control || !control.parent) return

      if (operation.op === 'add' || operation.op === 'replace') {
        control.markAsDirty()
        control.markAsTouched()
        control.setValue(operation.value)
      }

      control.parent.markAsDirty()
      control.parent.markAsTouched()
    })
  }
}
