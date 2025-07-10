import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiFade } from '@taiga-ui/kit'

@Component({
  selector: 'marketplace-additional-item',
  template: `
    <label tuiTitle>
      <span tuiSubtitle>{{ label }}</span>
      <span tuiFade>{{ data }}</span>
    </label>
    <tui-icon [icon]="icon" />
  `,
  styles: `
    :host {
      display: flex;
      justify-content: space-between;
      align-items: center;
      gap: 0.5rem;
      padding: 0.75rem 0.25rem;
      white-space: nowrap;

      &:hover {
        background-color: var(--tui-background-neutral-1);
      }

      [tuiSubtitle] {
        color: var(--tui-text-secondary);
      }

      tui-icon {
        opacity: 0.7;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, TuiTitle, TuiFade],
})
export class MarketplaceAdditionalItemComponent {
  @Input({ required: true })
  label!: string

  @Input({ required: true })
  icon!: string

  @Input({ required: true })
  data!: string
}
