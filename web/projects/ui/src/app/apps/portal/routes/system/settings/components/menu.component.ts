import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiAlertService, TuiLoaderModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { SettingsService } from '../settings.service'
import { SettingsSyncComponent } from './sync.component'
import { SettingsHttpsComponent } from './http.component'
import { SettingsButtonComponent } from './button.component'
import { SettingsUpdateComponent } from './update.component'

@Component({
  selector: 'settings-menu',
  template: `
    <ng-container *ngIf="server$ | async as server; else loading">
      <settings-sync *ngIf="!server['ntp-synced']" />
      <settings-http *ngIf="isTorHttp">
        <a
          tuiButton
          appearance="glass"
          iconRight="tuiIconExternalLinkLarge"
          target="_self"
          size="s"
          [href]="'https://' + server.ui.torHostname"
        >
          Open Https
        </a>
      </settings-http>
      <section *ngFor="let cat of service.settings | keyvalue : asIsOrder">
        <h3 class="g-title" (click)="addClick(cat.key)">{{ cat.key }}</h3>
        <ng-container *ngFor="let btn of cat.value">
          <settings-button [button]="btn">
            <div
              *ngIf="btn.title === 'Outbound Proxy'"
              tuiSubtitle
              [style.color]="
                !server.network.outboundProxy
                  ? 'var(--tui-warning-fill)'
                  : 'var(--tui-success-fill)'
              "
            >
              {{
                !server.network.outboundProxy
                  ? 'None'
                  : server.network.outboundProxy === 'primary'
                  ? 'System Primary'
                  : server.network.outboundProxy.proxyId
              }}
            </div>
          </settings-button>
          <settings-update
            *ngIf="btn.title === 'About'"
            [updated]="server['status-info'].updated"
          />
        </ng-container>
      </section>
    </ng-container>
    <ng-template #loading>
      <tui-loader
        textContent="Connecting to server"
        [style.margin-top.rem]="10"
      />
    </ng-template>
  `,
  styles: [
    `
      :host {
        display: flex;
        flex-direction: column;
        gap: 1rem;
        padding-top: 1rem;
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiLoaderModule,
    TuiButtonModule,
    SettingsSyncComponent,
    SettingsHttpsComponent,
    SettingsButtonComponent,
    SettingsUpdateComponent,
  ],
})
export class SettingsMenuComponent {
  private readonly clientStorageService = inject(ClientStorageService)
  private readonly alerts = inject(TuiAlertService)

  readonly isTorHttp = inject(ConfigService).isTorHttp()
  readonly server$ = inject(PatchDB<DataModel>).watch$('server-info')
  readonly service = inject(SettingsService)

  manageClicks = 0
  powerClicks = 0

  addClick(title: string) {
    switch (title) {
      case 'Security':
        this.addSecurityClick()
        break
      case 'Power':
        this.addPowerClick()
        break
      default:
        return
    }
  }

  asIsOrder() {
    return 0
  }

  private addSecurityClick() {
    this.manageClicks++

    if (this.manageClicks === 5) {
      this.manageClicks = 0
      this.alerts
        .open(
          this.clientStorageService.toggleShowDevTools()
            ? 'Dev tools unlocked'
            : 'Dev tools hidden',
        )
        .subscribe()
    }
  }

  private addPowerClick() {
    this.powerClicks++
    if (this.powerClicks === 5) {
      this.powerClicks = 0
      this.clientStorageService.toggleShowDiskRepair()
    }
  }
}
