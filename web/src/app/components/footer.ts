import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAnimated } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'

@Component({
  selector: '[appFooter]',
  template: `
    <button tuiButton type="reset" appearance="flat">Cancel</button>
    <button tuiButton>Save</button>
  `,
  host: { class: 'g-footer' },
  hostDirectives: [TuiAnimated],
  imports: [TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Footer {}
