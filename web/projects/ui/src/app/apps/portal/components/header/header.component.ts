import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink, RouterLinkActive } from '@angular/router'
import { HeaderConnectionComponent } from './connection.component'
import { HeaderHomeComponent } from './home.component'
import { HeaderCornerComponent } from './corner.component'
import { HeaderBreadcrumbComponent } from './breadcrumb.component'
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
        [routerLinkActiveOptions]="{ exact: true }"
        [headerBreadcrumb]="item"
      >
        <div class="plank"></div>
      </a>
    }
    <div [style.flex]="1"><div class="plank"></div></div>
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
          @include transition(clip-path);
          position: relative;
          margin-left: -1.25rem;
          backdrop-filter: blur(1rem);
          clip-path: var(--clip-path);
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
          opacity: 0.25;
        }

        &::before {
          @include transition(clip-path);
          content: '';
          position: absolute;
          inset: 0;
          clip-path: var(--clip-path);
          // TODO: Theme
          background: #5f5f5f;
          box-shadow: inset 0 1px rgb(255 255 255 / 25%);
        }
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterLink,
    RouterLinkActive,
    HeaderConnectionComponent,
    HeaderHomeComponent,
    HeaderCornerComponent,
    AsyncPipe,
    HeaderBreadcrumbComponent,
  ],
})
export class HeaderComponent {
  readonly breadcrumbs$ = inject(BreadcrumbsService)
}
