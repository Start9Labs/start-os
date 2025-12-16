import {
  ChangeDetectionStrategy,
  Component,
  inject,
  viewChild,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { i18nPipe } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { map, shareReplay } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { ServicesTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>{{ 'Installed services' | i18n }}</ng-container>

    <section class="g-card">
      <header>
        {{ 'Installed services' | i18n }}
      </header>

      <div #table [services]="services()"></div>
    </section>
  `,
  styles: `
    :host {
      padding: 1rem;
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
