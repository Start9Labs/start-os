import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'

export const NOT_PROVIDED = 'Not provided'
export interface AdditionalItem {
  name: i18nKey
  value: string
  icon?: string
  action?: () => void
}

@Component({
  selector: '[additionalItem]',
  template: `
    <span tuiTitle>
      <strong>{{ additionalItem.name | i18n }}</strong>
      <span tuiSubtitle>{{ additionalItem.value }}</span>
    </span>
    @if (icon) {
      <tui-icon [icon]="icon" />
    }
  `,
  styles: `
    :host._disabled {
      pointer-events: none;
      opacity: var(--tui-disabled-opacity);
    }
  `,
  host: {
    rel: 'noreferrer',
    target: '_blank',
    '[class._disabled]': 'disabled',
    '[attr.href]':
      'additionalItem.value.startsWith("http") ? additionalItem.value : null',
  },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, TuiTitle, i18nPipe],
})
export class ServiceAdditionalItemComponent {
  @Input({ required: true })
  additionalItem!: AdditionalItem

  get disabled(): boolean {
    return this.additionalItem.value === NOT_PROVIDED
  }

  get icon(): string | undefined {
    return this.additionalItem.value.startsWith('http')
      ? '@tui.external-link'
      : this.additionalItem.icon
  }
}
