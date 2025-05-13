import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { TuiComparator, TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiLoader } from '@taiga-ui/core'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getInstalledPrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { TitleDirective } from 'src/app/services/title.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { ServiceComponent } from './service.component'
import { ServicesService } from './services.service'
import { i18nPipe } from '@start9labs/shared'

@Component({
  template: `
    <ng-container *title>{{ 'Services' | i18n }}</ng-container>
    @if (!services()) {
      <tui-loader [style.height.%]="100" [textContent]="'Loading' | i18n" />
    } @else {
      @if (services()?.length) {
        <table tuiTable class="g-table" [(sorter)]="sorter">
          <thead>
            <tr>
              <th [style.width.rem]="3"></th>
              <th tuiTh [requiredSort]="true" [sorter]="name">
                {{ 'Name' | i18n }}
              </th>
              <th tuiTh>{{ 'Version' | i18n }}</th>
              <th tuiTh [requiredSort]="true" [sorter]="uptime">
                {{ 'Uptime' | i18n }}
              </th>
              <th tuiTh [requiredSort]="true" [sorter]="status">
                {{ 'Status' | i18n }}
              </th>
              <th [style.width.rem]="8" [style.text-indent.rem]="1.5"></th>
            </tr>
          </thead>
          <tbody>
            @for (pkg of services() | tuiTableSort; track $index) {
              <tr
                appService
                [pkg]="pkg"
                [depErrors]="errors()?.[(pkg | toManifest).id]"
              ></tr>
            }
          </tbody>
        </table>
      } @else {
        <section>
          <div>
            {{ 'Welcome to' | i18n }}
            <span>StartOS</span>
          </div>
          <p>
            {{
              'To get started, visit the Marketplace and download your first service'
                | i18n
            }}
          </p>
          <a tuiButton routerLink="../marketplace">
            {{ 'View Marketplace' | i18n }}
          </a>
        </section>
      }
    }
  `,
  styles: `
    :host {
      position: relative;
      font-size: 1rem;
      overflow: hidden;
    }

    :host-context(tui-root._mobile) {
      padding: 0;
    }

    section {
      height: 100%;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      text-align: center;

      div {
        font-size: min(12vw, 4rem);
        line-height: normal;
      }

      p {
        font-size: 1.5rem;
        line-height: 1.25em;
        padding: 0 1rem;
      }

      span {
        color: #ff4961;
      }

      a {
        margin-block-start: 1rem;
      }
    }
  `,
  host: { class: 'g-page' },
  imports: [
    ServiceComponent,
    ToManifestPipe,
    TuiTable,
    TitleDirective,
    i18nPipe,
    TuiLoader,
    TuiButton,
    RouterLink,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class DashboardComponent {
  readonly services = toSignal(inject(ServicesService))
  readonly errors = toSignal(inject(DepErrorService).depErrors$)

  readonly name: TuiComparator<PackageDataEntry> = (a, b) =>
    getManifest(b).title.toLowerCase() > getManifest(a).title.toLowerCase()
      ? -1
      : 1

  readonly status: TuiComparator<PackageDataEntry> = (a, b) =>
    getInstalledPrimaryStatus(b) > getInstalledPrimaryStatus(a) ? -1 : 1

  readonly uptime: TuiComparator<any> = (a, b) =>
    a.status.started || '' > a.status.started || '' ? -1 : 1

  sorter = this.name
}
