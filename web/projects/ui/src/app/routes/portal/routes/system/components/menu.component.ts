import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TuiAlertService, TuiLoader, TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { SystemService } from '../system.service'
import { SystemSyncComponent } from './sync.component'
import { SystemButtonComponent } from './button.component'
import { SystemUpdateComponent } from './update.component'

@Component({
  selector: 'system-menu',
  template: `
    @if (data(); as server) {
      @if (!server.ntpSynced) {
        <system-sync />
      }

      @for (cat of service.settings | keyvalue: asIsOrder; track $index) {
        <section class="g-card">
          <header (click)="addClick(cat.key)">{{ cat.key }}</header>
          @if (cat.key === 'General') {
            <system-update [updated]="server.statusInfo.updated" />
          }

          @for (btn of cat.value; track $index) {
            <system-button [button]="btn">
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
            </system-button>
          }
        </section>
      }
    } @else {
      <tui-loader
        textContent="Connecting to server"
        [style.margin-top.rem]="10"
      />
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiLoader,
    SystemSyncComponent,
    SystemButtonComponent,
    SystemUpdateComponent,
  ],
})
export class SystemMenuComponent {
  private readonly clientStorageService = inject(ClientStorageService)
  private readonly alerts = inject(TuiAlertService)

  readonly service = inject(SystemService)
  readonly data = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('serverInfo'),
  )

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
