import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiSvgModule } from '@taiga-ui/core'
import { AdditionalItem, FALLBACK_URL } from '../pipes/to-additional.pipe'

@Component({
  selector: '[additionalItem]',
  template: `
    <div [style.flex]="1">
      <strong>{{ additionalItem.name }}</strong>
      <div>{{ additionalItem.description }}</div>
    </div>
    <tui-svg *ngIf="icon" [src]="icon"></tui-svg>
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
  imports: [CommonModule, TuiSvgModule],
})
export class ServiceAdditionalItemComponent {
  @Input({ required: true })
  additionalItem!: AdditionalItem

  get disabled(): boolean {
    return this.additionalItem.description === FALLBACK_URL
  }

  get icon(): string | undefined {
    return this.additionalItem.description.startsWith('http')
      ? 'tuiIconExternalLinkLarge'
      : this.additionalItem.icon
  }
}
