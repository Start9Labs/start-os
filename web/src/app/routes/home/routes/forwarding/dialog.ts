import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { TuiAutoFocus, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiTextfield,
} from '@taiga-ui/core'
import { TuiChevron, TuiDataListWrapper, TuiSelect } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext } from '@taiga-ui/polymorpheus'
import { Forwarding } from 'src/app/routes/home/routes/forwarding/service'

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <tui-textfield>
        <label tuiLabel>Purpose</label>
        <input tuiInput tuiAutoFocus formControlName="purpose" />
      </tui-textfield>
      <tui-error formControlName="purpose" />
      <tui-textfield tuiChevron [tuiTextfieldCleaner]="false">
        <label tuiLabel>Protocol</label>
        <input tuiSelect formControlName="protocol" />
        <tui-data-list-wrapper
          *tuiDropdown
          [items]="['TCP/UDP', 'TCP', 'UDP']"
        />
      </tui-textfield>
      <tui-textfield tuiChevron [content]="ip" [tuiTextfieldCleaner]="false">
        <label tuiLabel>IP Address</label>
        <input tuiSelect formControlName="ip" />
        <tui-data-list-wrapper *tuiDropdown [items]="ips" [itemContent]="ip" />
        <ng-template #ip let-value>
          <span class="ip">
            <span>{{ names[value] }}</span>
            <span class="g-secondary">{{ value }}</span>
          </span>
        </ng-template>
      </tui-textfield>
      <tui-error formControlName="ip" />
      <tui-textfield>
        <label tuiLabel>External Port</label>
        <input tuiInput formControlName="external" />
      </tui-textfield>
      <tui-error formControlName="external" />
      <tui-error class="g-secondary" error="A port or port range" />
      <tui-textfield>
        <label tuiLabel>Internal Port</label>
        <input tuiInput formControlName="internal" />
      </tui-textfield>
      <tui-error formControlName="internal" />
      <tui-error
        class="g-secondary"
        error="A port or port range. Must be equal in size to External port range"
      />
      <footer>
        <button
          tuiButton
          appearance="flat"
          type="button"
          (click)="context.$implicit.complete()"
        >
          Cancel
        </button>
        <button tuiButton>Save</button>
      </footer>
    </form>
  `,
  styles: `
    .ip {
      display: flex;
      flex: 1;
      white-space: nowrap;
      gap: 1rem;
      overflow: hidden;

      :first-child {
        min-width: 0;
        flex: 1;
        overflow: hidden;
        text-overflow: ellipsis;
      }
    }
  `,
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiTextfield,
    TuiInput,
    TuiSelect,
    TuiDataListWrapper,
    TuiError,
    TuiButton,
    TuiChevron,
    TuiAutoFocus,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ForwardingDialog {
  protected readonly context = injectContext<TuiDialogContext<Forwarding>>()
  protected readonly form = inject(NonNullableFormBuilder).group({
    purpose: ['', Validators.required],
    protocol: ['TCP/UDP', Validators.required],
    ip: ['', Validators.required],
    external: ['', Validators.required],
    internal: ['', Validators.required],
  })

  protected readonly ips = ['192.168.237.42', '192.168.0.69']
  protected readonly names: Record<string, string> = {
    '192.168.237.42': 'Device A',
    '192.168.0.69':
      'A very very long device name that is never gonna fit and therefore should be truncated',
  }

  protected save() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
    } else {
      this.context.completeWith(this.form.getRawValue())
    }
  }
}
