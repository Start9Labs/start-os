import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  IsActiveMatchOptions,
  RouterLink,
  RouterLinkActive,
} from '@angular/router'
import { PatchDB } from 'patch-db-client'
import { BreadcrumbsService } from 'src/app/services/breadcrumbs.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderMenuComponent } from './menu.component'
import { HeaderMobileComponent } from './mobile.component'
import { HeaderNavigationComponent } from './navigation.component'
import { HeaderSnekDirective } from './snek.directive'
import { HeaderStatusComponent } from './status.component'

@Component({
  selector: 'header[appHeader]',
  template: `
    <header-navigation />
    <div class="item item_center" [headerMobile]="breadcrumbs$ | async">
      <img
        [appSnek]="snekScore()"
        class="snek"
        alt="Play Snake"
        src="assets/img/icons/snek.png"
      />
    </div>
    <header-status class="item item_connection" />
    <header-menu class="item item_corner" />
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        display: flex;
        height: 2.75rem;
        border-radius: var(--bumper);
        margin: var(--bumper);
        overflow: hidden;

        .item {
          position: relative;
          border-radius: inherit;
          isolation: isolate;

          &::before {
            @include transition(all);
            content: '';
            position: absolute;
            inset: 0;
            border-radius: inherit;
            backdrop-filter: blur(1rem);
            transform: skewX(30deg);
            background: rgb(75 75 75 / 65%);
            box-shadow: inset 0 1px rgb(255 255 255 / 25%);
            z-index: -1;
          }

          &_center {
            flex: 1;
          }

          &_connection::before {
            box-shadow:
              inset 0 1px rgba(255, 255, 255, 0.25),
              inset 0 -0.75rem 0 -0.5rem var(--status);
          }

          &_corner {
            margin-inline-start: var(--bumper);

            &::before {
              right: -2rem;
            }
          }
        }

        &:has([data-status='error']) {
          --status: var(--tui-status-negative);
        }

        &:has([data-status='warning']) {
          --status: var(--tui-status-warning);
        }

        &:has([data-status='neutral']) {
          --status: var(--tui-status-neutral);
        }

        &:has([data-status='success']) {
          --status: var(--tui-status-positive);
        }
      }

      .snek {
        @include center-top();
        @include transition(opacity);
        right: 2rem;
        width: 1rem;
        opacity: 0.2;
        cursor: pointer;

        &:hover {
          opacity: 1;
        }
      }

      :host-context(tui-root._mobile) {
        .item_center::before {
          left: -2rem;
        }
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterLink,
    RouterLinkActive,
    AsyncPipe,
    HeaderStatusComponent,
    HeaderNavigationComponent,
    HeaderSnekDirective,
    HeaderMobileComponent,
    HeaderMenuComponent,
  ],
})
export class HeaderComponent {
  readonly options = OPTIONS
  readonly breadcrumbs$ = inject(BreadcrumbsService)
  readonly snekScore = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$(
      'ui',
      'gaming',
      'snake',
      'highScore',
    ),
    { initialValue: 0 },
  )
}

const OPTIONS: IsActiveMatchOptions = {
  paths: 'exact',
  queryParams: 'ignored',
  fragment: 'ignored',
  matrixParams: 'ignored',
}
