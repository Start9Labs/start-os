import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { TuiAnimated } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'

@Component({
  selector: '[appFooter]',
  template: `
    <button tuiButton type="reset" appearance="flat" [disabled]="disabled()">
      Cancel
    </button>
    <button tuiButton [disabled]="disabled()">Save</button>
    <ng-content />
  `,
  host: { class: 'g-footer' },
  hostDirectives: [TuiAnimated],
  imports: [TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Footer {
  readonly disabled = input.required<boolean>()
}
