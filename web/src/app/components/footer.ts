import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { FormGroupDirective } from '@angular/forms'
import { TuiAnimated } from '@taiga-ui/cdk'
import { TuiButton, TuiHint } from '@taiga-ui/core'

@Component({
  selector: '[appFooter]',
  template: `
    <button
      tuiButton
      type="reset"
      appearance="flat"
      [disabled]="form?.pristine"
    >
      Cancel
    </button>
    <button
      tuiButton
      [disabled]="form?.pristine || blocked()"
      [tuiHint]="blocked()"
    >
      Save
    </button>
    <ng-content />
    @if (form?.control?.events | async) {}
  `,
  host: { class: 'g-footer' },
  hostDirectives: [TuiAnimated],
  imports: [TuiButton, TuiHint, AsyncPipe],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Footer {
  readonly blocked = input<string | null>(null)
  protected readonly form = inject(FormGroupDirective, { optional: true })
}
