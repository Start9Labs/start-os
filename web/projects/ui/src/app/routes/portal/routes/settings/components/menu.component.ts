import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiAlertService, TuiLoader, TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { SettingsService } from '../settings.service'
import { SettingsSyncComponent } from './sync.component'
import { SettingsButtonComponent } from './button.component'
import { SettingsUpdateComponent } from './update.component'

@Component({
  selector: 'settings-menu',
  template: `
    <ng-container *ngIf="server$ | async as server; else loading">
      <settings-sync *ngIf="!server.ntpSynced" />
      <section *ngFor="let cat of service.settings | keyvalue: asIsOrder">
        <h3 class="g-title" (click)="addClick(cat.key)">{{ cat.key }}</h3>
        <settings-update
          *ngIf="cat.key === 'General'"
          [updated]="server.statusInfo.updated"
        />
        <ng-container *ngFor="let btn of cat.value">
          <settings-button [button]="btn">
            <!-- // @TODO 041
            <div
              *ngIf="btn.title === 'Outbound Proxy'"
              tuiSubtitle
              [style.color]="
                !server.network.outboundProxy
                  ? 'var(--tui-status-warning)'
                  : 'var(--tui-status-positive)'
              "
            >
              {{ server.network.outboundProxy || 'None' }}
            </div> -->
          </settings-button>
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
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiLoader,
    TuiButton,
    SettingsSyncComponent,
    SettingsButtonComponent,
    SettingsUpdateComponent,
  ],
})
export class SettingsMenuComponent {
  private readonly clientStorageService = inject(ClientStorageService)
  private readonly alerts = inject(TuiAlertService)

  readonly server$ = inject<PatchDB<DataModel>>(PatchDB).watch$('serverInfo')
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
