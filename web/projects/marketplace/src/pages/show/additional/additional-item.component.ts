import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiLabelModule, TuiSvgModule } from '@taiga-ui/core'
import { TuiLineClampModule } from '@taiga-ui/kit'

@Component({
  selector: 'marketplace-additional-item',
  template: `
    <div class="item-container">
      <label [tuiLabel]="label">
        <tui-line-clamp [content]="data" [linesLimit]="1"></tui-line-clamp>
      </label>
      <tui-svg [src]="icon"></tui-svg>
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

        tui-svg {
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
  imports: [CommonModule, TuiSvgModule, TuiLineClampModule, TuiLabelModule],
})
export class MarketplaceAdditionalItemComponent {
  @Input({ required: true })
  label!: string

  @Input({ required: true })
  icon!: string

  @Input({ required: true })
  data!: string
}
