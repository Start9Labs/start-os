import { TuiTextfieldControllerModule } from '@taiga-ui/legacy'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { RouterInfoComponent } from './info.component'
import { PrimaryIpPipe } from './primary-ip.pipe'
import { RouterPortComponent } from './table.component'

@Component({
  template: `
    <ng-container *ngIf="server$ | async as server">
      <router-info [enabled]="!server.network.wanConfig.upnp" />
      <table
        *ngIf="server.host.hostnameInfo[80] | primaryIp as ip"
        tuiTextfieldAppearance="unstyled"
        tuiTextfieldSize="m"
        [tuiTextfieldLabelOutside]="true"
      >
        <thead>
          <tr>
            <th [style.width.rem]="2.5"></th>
            <th [style.padding-left.rem]="0.75">
              <div class="g-title">Port</div>
            </th>
            <th>
              <div class="g-title">Target</div>
            </th>
            <th [style.width.rem]="3"></th>
          </tr>
        </thead>
        <tbody>
          <tr
            *ngFor="let portForward of server.network.wanConfig.forwards"
            [portForward]="portForward"
            [ip]="ip"
          ></tr>
        </tbody>
      </table>
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  styles: `
    table {
      width: 100%;
      min-width: 30rem;
      max-width: 40rem;
      table-layout: fixed;
      background: var(--tui-background-base-alt);
      border-radius: 0.75rem;
      font-size: 1rem;
      margin: 2rem 0;
      box-shadow: 0 1rem var(--tui-background-base-alt);
    }
  `,
  imports: [
    CommonModule,
    RouterInfoComponent,
    RouterPortComponent,
    TuiTextfieldControllerModule,
    PrimaryIpPipe,
  ],
})
export default class SystemRouterComponent {
  readonly server$ = inject<PatchDB<DataModel>>(PatchDB).watch$('serverInfo')
}
