import {
  ChangeDetectionStrategy,
  Component,
  HostBinding,
  inject,
  Input,
} from '@angular/core'
import { Breadcrumb } from 'src/app/services/breadcrumbs.service'
import { TuiIconModule, TuiTitleModule } from '@taiga-ui/experimental'
import {
  TUI_ANIMATION_OPTIONS,
  tuiFadeIn,
  tuiWidthCollapse,
} from '@taiga-ui/core'

@Component({
  standalone: true,
  selector: 'a[headerBreadcrumb]',
  template: `
    @if (item.icon?.startsWith('tuiIcon')) {
      <tui-icon [icon]="item.icon || ''" />
    } @else if (item.icon) {
      <img [style.width.rem]="2" [src]="item.icon" [alt]="item.title" />
    }
    <span tuiTitle>
      {{ item.title }}
      @if (item.subtitle) {
        <span tuiSubtitle="">{{ item.subtitle }}</span>
      }
    </span>
    <ng-content />
  `,
  styles: [
    `
      :host {
        display: flex;
        align-items: center;
        gap: 1rem;
        min-width: 1.25rem;
        white-space: nowrap;
        text-transform: capitalize;
        --clip-path: polygon(
          calc(100% - 1.75rem) 0%,
          calc(100% - 0.875rem) 50%,
          100% 100%,
          0% 100%,
          0.875rem 50%,
          0% 0%
        );

        &:not(.active) {
          --clip-path: polygon(
            calc(100% - 1.75rem) 0%,
            calc(100% - 0.875rem) 50%,
            calc(100% - 1.75rem) 100%,
            0% 100%,
            0.875rem 50%,
            0% 0%
          );
        }

        & > * {
          font-weight: bold;
          gap: 0;
          border-radius: 100%;
        }

        &::before,
        &::after {
          content: '';
          margin: 0.5rem;
        }

        &::before {
          margin: 0.25rem;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIconModule, TuiTitleModule],
  animations: [tuiWidthCollapse, tuiFadeIn],
})
export class HeaderBreadcrumbComponent {
  @Input({ required: true, alias: 'headerBreadcrumb' })
  item!: Breadcrumb

  @HostBinding('@tuiFadeIn')
  @HostBinding('@tuiWidthCollapse')
  readonly animation = inject(TUI_ANIMATION_OPTIONS)
}
