import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { CopyService } from '@start9labs/shared'
import { mask } from 'src/app/util/mask'
import { TuiButtonModule, TuiLabelModule } from '@taiga-ui/core'

@Component({
  selector: 'service-credential',
  template: `
    <label [style.flex]="1" [tuiLabel]="label">
      {{ masked ? mask : value }}
    </label>
    <button
      tuiIconButton
      appearance="flat"
      [icon]="masked ? 'tuiIconEyeLarge' : 'tuiIconEyeOffLarge'"
      (click)="masked = !masked"
    >
      Toggle
    </button>
    <button
      tuiIconButton
      appearance="flat"
      icon="tuiIconCopyLarge"
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
          box-shadow: 0 1px var(--tui-clear);
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButtonModule, TuiLabelModule],
})
export class ServiceCredentialComponent {
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