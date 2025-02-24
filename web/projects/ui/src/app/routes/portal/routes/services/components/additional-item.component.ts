import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'

export const FALLBACK_URL = 'Not provided'
export interface AdditionalItem {
  name: string
  description: string
  icon?: string
  action?: () => void
}

@Component({
  selector: '[additionalItem]',
  template: `
    <span tuiTitle>
      <strong>{{ additionalItem.name }}</strong>
      <span tuiSubtitle>{{ additionalItem.description }}</span>
    </span>
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
  imports: [TuiIcon, TuiTitle],
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
