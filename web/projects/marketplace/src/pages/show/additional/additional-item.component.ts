import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIcon, TuiLabel, TuiTitle } from '@taiga-ui/core'
import { TuiLineClamp } from '@taiga-ui/kit'

@Component({
  selector: 'marketplace-additional-item',
  template: `
    <div class="item-container">
      <label tuiTitle>
        <span tuiSubtitle>{{ label }}</span>
        <tui-line-clamp [content]="data" [linesLimit]="1" />
      </label>
      <tui-icon [icon]="icon" />
    </div>
  `,
  styles: [
    `
      .item-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 0.75rem 0.25rem;

        &:hover {
          background-color: rgb(113 113 122 / 0.1);
        }

        [tuiSubtitle] {
          color: var(--tui-text-secondary);
        }

        tui-icon {
          opacity: 0.7;
        }
      }

      ::ng-deep .t-text {
        font-family: 'Montserrat';
        font-weight: 600;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiLineClamp, TuiLabel, TuiIcon, TuiTitle],
})
export class MarketplaceAdditionalItemComponent {
  @Input({ required: true })
  label!: string

  @Input({ required: true })
  icon!: string

  @Input({ required: true })
  data!: string
}
