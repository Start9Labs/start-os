import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIconModule } from '@taiga-ui/experimental'
import { Breadcrumb } from '../../services/breadcrumbs.service'
import { RouterLink } from '@angular/router'

@Component({
  standalone: true,
  selector: '[headerMobile]',
  template: `
    @if (headerMobile?.length) {
      <a [routerLink]="back" [style.padding.rem]="0.75">
        <tui-icon icon="tuiIconArrowLeft" />
      </a>
    }
    <span class="title">{{ title }}</span>
    <ng-content />
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        display: flex;
        align-items: center;
        font-size: 1rem;

        > * {
          display: none;
        }
      }

      :host-context(tui-root._mobile) {
        margin: 0;
        --clip-path: polygon(
          0% 0%,
          calc(100% - 1.75rem) 0%,
          100% 100%,
          0% 100%
        );

        > * {
          display: block;
        }
      }

      .title {
        @include text-overflow();
        max-width: calc(100% - 5rem);
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIconModule, RouterLink],
})
export class HeaderMobileComponent {
  @Input() headerMobile: readonly Breadcrumb[] | null = []

  get title() {
    return this.headerMobile?.[this.headerMobile?.length - 1]?.title || ''
  }

  get back() {
    return (
      this.headerMobile?.[this.headerMobile?.length - 2]?.routerLink ||
      '/portal/dashboard'
    )
  }
}
