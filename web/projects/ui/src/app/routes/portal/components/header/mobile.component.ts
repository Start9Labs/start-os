import { TuiIcon } from '@taiga-ui/core'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { WA_WINDOW } from '@ng-web-apis/common'
import { Breadcrumb } from 'src/app/services/breadcrumbs.service'

@Component({
  standalone: true,
  selector: '[headerMobile]',
  template: `
    @if (headerMobile && headerMobile.length > 1) {
      <a [routerLink]="back" [style.padding.rem]="0.75">
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

      .title {
        @include text-overflow();
        max-width: calc(100% - 5rem);
        text-transform: capitalize;

        &:first-child {
          margin-inline-start: 1rem;
        }
      }

      :host-context(tui-root._mobile) {
        > * {
          display: block;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, RouterLink],
})
export class HeaderMobileComponent {
  private readonly win = inject(WA_WINDOW)

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
      '/portal/services'
    )
  }
}
