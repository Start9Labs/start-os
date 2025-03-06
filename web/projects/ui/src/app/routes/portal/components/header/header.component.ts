import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnInit,
  viewChild,
  ViewContainerRef,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleService } from 'src/app/services/title.service'
import { HeaderMenuComponent } from './menu.component'
import { HeaderNavigationComponent } from './navigation.component'
import { HeaderSnekDirective } from './snek.directive'
import { HeaderStatusComponent } from './status.component'

@Component({
  selector: 'header[appHeader]',
  template: `
    <header-navigation />
    <div class="item item_center">
      <div class="mobile"><ng-container #vcr /></div>
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

        .mobile {
          display: none;
        }

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
            background: color-mix(
              in hsl,
              var(--start9-base-2) 75%,
              transparent
            );
            box-shadow: inset 0 1px rgb(255 255 255 / 25%);
            z-index: -1;
          }

          &_center {
            flex: 1;
            min-width: 0;
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

        .mobile {
          display: flex;
          height: 100%;
          align-items: center;
          font: var(--tui-font-text-l);
          padding: 1rem;
          white-space: nowrap;
          overflow: hidden;

          ::ng-deep > [tuiIconButton] {
            margin-inline-start: -1rem;
          }
        }
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    HeaderStatusComponent,
    HeaderNavigationComponent,
    HeaderSnekDirective,
    HeaderMenuComponent,
  ],
})
export class HeaderComponent implements OnInit {
  private readonly title = inject(TitleService)

  readonly vcr = viewChild.required('vcr', { read: ViewContainerRef })
  readonly snekScore = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$(
      'ui',
      'gaming',
      'snake',
      'highScore',
    ),
    { initialValue: 0 },
  )

  ngOnInit() {
    this.title.register(this.vcr())
  }
}
