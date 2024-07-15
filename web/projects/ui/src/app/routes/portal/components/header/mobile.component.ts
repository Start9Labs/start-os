import { TuiIcon } from '@taiga-ui/core'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { WINDOW } from '@ng-web-apis/common'
import { Breadcrumb } from 'src/app/services/breadcrumbs.service'

@Component({
  standalone: true,
  selector: '[headerMobile]',
  template: `
    @if (headerMobile && headerMobile.length > 1) {
      <a
        [routerLink]="back"
        [style.padding.rem]="0.75"
        [queryParams]="queryParams"
      >
        <tui-icon icon="@tui.arrow-left" />
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
        text-transform: capitalize;

        &:first-child {
          margin-inline-start: 1rem;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, RouterLink],
})
export class HeaderMobileComponent {
  private readonly win = inject(WINDOW)

  @Input() headerMobile: readonly Breadcrumb[] | null = []

  get title() {
    return (
      this.headerMobile?.[this.headerMobile?.length - 1]?.title ||
      (this.win.location.search ? 'Utilities' : 'Services')
    )
  }

  get back() {
    return (
      this.headerMobile?.[this.headerMobile?.length - 2]?.routerLink ||
      '/portal/dashboard'
    )
  }

  get queryParams() {
    return this.back === '/portal/dashboard' ? { tab: 'utilities' } : null
  }
}
