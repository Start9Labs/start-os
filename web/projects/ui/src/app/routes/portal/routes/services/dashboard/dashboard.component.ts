import {
  ChangeDetectionStrategy,
  Component,
  inject,
  viewChild,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { i18nPipe } from '@start9labs/shared'
import { TuiTable } from '@taiga-ui/addon-table'
import { PatchDB } from 'patch-db-client'
import { map, shareReplay } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { getManifest } from 'src/app/utils/get-package-data'
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

      section {
        padding-block: 0;
      }
    }
  `,
  host: { class: 'g-page' },
  imports: [TuiTable, TitleDirective, i18nPipe, ServicesTableComponent],
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
