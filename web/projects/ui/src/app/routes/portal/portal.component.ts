import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterOutlet } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiButton, TuiIcon, TuiLoader, TuiScrollbar } from '@taiga-ui/core'
import { TuiActionBar, TuiProgress } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { TabsComponent } from 'src/app/routes/portal/components/tabs.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { OSService } from 'src/app/services/os.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderComponent } from './components/header/header.component'

@Component({
  template: `
    <header appHeader>{{ name() }}</header>
    <main>
      <tui-scrollbar [style.max-height.%]="100">
        <router-outlet />
      </tui-scrollbar>
    </main>
    <app-tabs />
    @if (update(); as update) {
      <tui-action-bar *tuiActionBar="bar()">
        @if (update === true) {
          <tui-icon icon="@tui.check" class="g-positive" />
          Download complete, restart to apply changes
        } @else if (
          update.overall && update.overall !== true && update.overall.total
        ) {
          <tui-progress-circle
            size="xxs"
            [style.display]="'flex'"
            [max]="100"
            [value]="getProgress(update.overall.total, update.overall.done)"
          />
          Downloading:
          {{ getProgress(update.overall.total, update.overall.done) }}%
        } @else {
          <tui-loader />
          Calculating download size
        }
        @if (update === true) {
          <button tuiButton size="s" (click)="restart()">Restart</button>
        }
      </tui-action-bar>
    }
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    :host {
      height: 100%;
      display: flex;
      flex-direction: column;
      // @TODO Theme
      background: url(/assets/img/background_dark.jpeg) fixed center/cover;

      &::before {
        content: '';
        position: fixed;
        inset: 0;
        backdrop-filter: blur(0.5rem);
      }
    }

    main {
      flex: 1;
      overflow: hidden;
      margin: 0 var(--bumper) var(--bumper);
      filter: grayscale(1) brightness(0.75);

      @include taiga.transition(filter);

      header:has([data-status='success']) + & {
        filter: none;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterOutlet,
    HeaderComponent,
    TabsComponent,
    TuiScrollbar,
    TuiActionBar,
    TuiProgress,
    TuiLoader,
    TuiIcon,
    TuiButton,
  ],
})
export class PortalComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)

  readonly name = toSignal(this.patch.watch$('ui', 'name'))
  readonly update = toSignal(inject(OSService).updating$)
  readonly bar = signal(true)

  getProgress(size: number, downloaded: number): number {
    return Math.round((100 * downloaded) / (size || 1))
  }

  async restart() {
    const loader = this.loader.open('Beginning restart').subscribe()

    try {
      this.bar.set(false)
      await this.api.restartServer({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
