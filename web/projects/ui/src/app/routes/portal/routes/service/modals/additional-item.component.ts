import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIcon } from '@taiga-ui/core'
import {
  AdditionalItem,
  FALLBACK_URL,
} from 'src/app/routes/portal/routes/service/pipes/to-additional.pipe'

@Component({
  selector: '[additionalItem]',
  template: `
    <div [style.flex]="1">
      <strong>{{ additionalItem.name }}</strong>
      <div>{{ additionalItem.description }}</div>
    </div>
    @if (icon) {
      <tui-icon [icon]="icon" />
    }
  `,
  styles: [
    `
      :host._disabled {
        pointer-events: none;
        opacity: var(--tui-disabled-opacity);
      }
    `,
  ],
  host: {
    rel: 'noreferrer',
    target: '_blank',
    '[class._disabled]': 'disabled',
    '[attr.href]':
      'additionalItem.description.startsWith("http") ? additionalItem.description : null',
  },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiIcon],
})
export class ServiceAdditionalItemComponent {
  @Input({ required: true })
  additionalItem!: AdditionalItem

  get disabled(): boolean {
    return this.additionalItem.description === FALLBACK_URL
  }

  get icon(): string | undefined {
    return this.additionalItem.description.startsWith('http')
      ? '@tui.external-link'
      : this.additionalItem.icon
  }
}
