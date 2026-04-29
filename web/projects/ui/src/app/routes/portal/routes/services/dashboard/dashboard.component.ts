import {
  ChangeDetectionStrategy,
  Component,
  inject,
  viewChild,
} from '@angular/core'
import { TitlePipe } from '@angular/common'
import { toSignal } from '@angular/core/rxjs-interop'
import { i18nPipe } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { map, shareReplay } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { ServicesTableComponent } from './table.component'
import { ServicesPreferencesService } from 'src/app/services/services-preferences.service'
import { PrimaryStatus, PrimaryRendering } from 'src/app/services/pkg-status-rendering.service'
import { TuiTextfield } from '@taiga-ui/kit'
import { TuiButton } from '@taiga-ui/core'

@Component({
  template: `
    <ng-container *title>{{ 'Installed services' | i18n }}</ng-container>

    <section class="g-card">
      <header>
        {{ 'Installed services' | i18n }}
      </header>

      <div class="filter-bar">
        <input
          type="text"
          tuiTextfield
          placeholder="Search services..."
          [value]="prefs.filterState$.value.query"
          (input)="prefs.setQuery($any($event.target).value)"
        />
        <div class="state-toggles">
          @for (state of ALL_STATES; track state) {
            <button
              tuiButton
              size="s"
              [appearance]="prefs.filterState$.value.states.includes(state) ? 'primary' : 'secondary'"
              (click)="prefs.toggleState(state)"
            >
              {{ state | titlecase }}
            </button>
          }
        </div>
      </div>

      <div #table [services]="services()"></div>
    </section>
  `,
  styles: `
    :host {
      padding: 1rem;
    }

    .filter-bar {
      display: flex;
      gap: 0.5rem;
      padding: 0.75rem 1rem;
      border-bottom: 1px solid var(--tui-base-03);
    }

    .filter-bar input {
      flex: 1;
    }

    .state-toggles {
      display: flex;
      gap: 0.25rem;
      flex-wrap: wrap;
    }

    :host-context(tui-root._mobile) {
      header {
        display: none;
      }

      .g-card {
        padding: 0;
        margin-top: -0.75rem;
        background: none;
        box-shadow: none;
      }
    }
  `,
  host: { class: 'g-page' },
  imports: [TitleDirective, i18nPipe, ServicesTableComponent],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class DashboardComponent {
  protected readonly prefs = inject(ServicesPreferencesService)
  protected readonly ALL_STATES = Object.keys(PrimaryRendering) as PrimaryStatus[]

  readonly services = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('packageData')
      .pipe(
        map(pkgs => Object.values(pkgs)),
        shareReplay(1),
      ),
    { initialValue: null },
  )

  protected _ = viewChild<ServicesTableComponent<any>>('table')
}
