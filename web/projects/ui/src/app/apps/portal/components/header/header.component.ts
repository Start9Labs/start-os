import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  IsActiveMatchOptions,
  RouterLink,
  RouterLinkActive,
} from '@angular/router'
import { PatchDB } from 'patch-db-client'
import { HeaderConnectionComponent } from './connection.component'
import { HeaderHomeComponent } from './home.component'
import { HeaderCornerComponent } from './corner.component'
import { HeaderBreadcrumbComponent } from './breadcrumb.component'
import { HeaderSnekDirective } from './snek.directive'
import { HeaderMobileComponent } from './mobile.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { BreadcrumbsService } from '../../services/breadcrumbs.service'

@Component({
  selector: 'header[appHeader]',
  template: `
    <a headerHome routerLink="/portal/desktop" routerLinkActive="active">
      <div class="plank"></div>
    </a>
    @for (item of breadcrumbs$ | async; track $index) {
      <a
        routerLinkActive="active"
        [routerLink]="item.routerLink"
        [routerLinkActiveOptions]="options"
        [headerBreadcrumb]="item"
      >
        <div class="plank"></div>
      </a>
    }
    <div [style.flex]="1" [headerMobile]="breadcrumbs$ | async">
      <div class="plank"></div>
      <img
        [appSnek]="(snekScore$ | async) || 0"
        class="snek"
        alt="Play Snake"
        src="assets/img/icons/snek.png"
      />
    </div>
    <header-connection><div class="plank"></div></header-connection>
    <header-corner><div class="plank"></div></header-corner>
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        display: flex;
        height: 3.5rem;
        padding: 0.375rem;
        --clip-path: polygon(
          0% 0%,
          calc(100% - 1.75rem) 0%,
          100% 100%,
          1.75rem 100%
        );

        > * {
          @include transition(all);
          position: relative;
          margin-left: -1.25rem;
          backdrop-filter: blur(1rem);
          clip-path: var(--clip-path);

          &:active {
            backdrop-filter: blur(2rem) brightness(0.75) saturate(0.75);
          }
        }

        &:has([data-connection='error']) {
          --status: var(--tui-error-fill);
        }

        &:has([data-connection='warning']) {
          --status: var(--tui-warning-fill);
        }

        &:has([data-connection='neutral']) {
          --status: var(--tui-neutral-fill);
        }

        &:has([data-connection='success']) {
          --status: var(--tui-success-fill);
        }
      }

      :host-context(tui-root._mobile) {
        a {
          display: none;
        }

        header-corner .plank::before {
          box-shadow:
            inset 0 1px rgb(255 255 255 / 25%),
            inset -0.375rem 0 var(--status);
        }
      }

      .plank {
        @include transition(opacity);
        position: absolute;
        inset: 0;
        z-index: -1;
        filter: url(#round-corners);
        opacity: 0.5;

        .active & {
          opacity: 0.75;

          &::before {
            // TODO: Theme
            background: #363636;
          }
        }

        &::before {
          @include transition(all);
          content: '';
          position: absolute;
          inset: 0;
          clip-path: var(--clip-path);
          // TODO: Theme
          background: #5f5f5f;
          box-shadow: inset 0 1px rgb(255 255 255 / 25%);
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
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterLink,
    RouterLinkActive,
    AsyncPipe,
    HeaderConnectionComponent,
    HeaderHomeComponent,
    HeaderCornerComponent,
    HeaderSnekDirective,
    HeaderBreadcrumbComponent,
    HeaderMobileComponent,
  ],
})
export class HeaderComponent {
  readonly options = OPTIONS
  readonly breadcrumbs$ = inject(BreadcrumbsService)
  readonly snekScore$ = inject(PatchDB<DataModel>).watch$(
    'ui',
    'gaming',
    'snake',
    'high-score',
  )
}

const OPTIONS: IsActiveMatchOptions = {
  paths: 'exact',
  queryParams: 'ignored',
  fragment: 'ignored',
  matrixParams: 'ignored',
}
