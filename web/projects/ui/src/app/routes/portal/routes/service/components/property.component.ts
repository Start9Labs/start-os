import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { CopyService } from '@start9labs/shared'
import { TuiButton, TuiLabel, TuiTitle } from '@taiga-ui/core'
import { mask } from 'src/app/utils/mask'

@Component({
  selector: 'service-property',
  template: `
    <label [style.flex]="1" tuiTitle>
      <span tuiSubtitle>{{ label }}</span>
      {{ masked ? mask : value }}
    </label>
    <button
      tuiIconButton
      appearance="flat"
      [iconStart]="masked ? '@tui.eye' : '@tui.eye-off'"
      (click)="masked = !masked"
    >
      Toggle
    </button>
    <button
      tuiIconButton
      appearance="flat"
      iconStart="@tui.copy"
      (click)="copyService.copy(value)"
    >
      Copy
    </button>
  `,
  styles: [
    `
      :host {
        display: flex;
        padding: 0.5rem 0;

        &:not(:last-of-type) {
          box-shadow: 0 1px var(--tui-background-neutral-1);
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButton, TuiLabel, TuiTitle],
})
export class ServicePropertyComponent {
  @Input()
  label = ''

  @Input()
  value = ''

  masked = true

  readonly copyService = inject(CopyService)

  get mask(): string {
    return mask(this.value, 64)
  }
}
