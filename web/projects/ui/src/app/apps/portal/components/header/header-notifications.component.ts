import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiForModule } from '@taiga-ui/cdk'
import { TuiScrollbarModule } from '@taiga-ui/core'
import {
  TuiAvatarStackModule,
  TuiButtonModule,
  TuiCellModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { first } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderNotificationComponent } from './header-notification.component'
import { toRouterLink } from '../../utils/to-router-link'
import { HeaderNotificationsService } from './header-notifications.service'

@Component({
  selector: 'header-notifications',
  template: `
    <ng-container *ngIf="notifications$ | async as notifications">
      <h3 class="g-title" style="padding: 0 1rem">
        Notifications
        <a
          *ngIf="notifications.length"
          style="margin-left: auto; text-transform: none; font-size: 0.8rem;"
          (click)="service.markAllSeen(notifications[0].id)"
        >
          Mark all as seen
        </a>
      </h3>
      <tui-scrollbar *ngIf="packageData$ | async as packageData">
        <header-notification
          *ngFor="let not of notifications; let i = index; empty: blank"
          tuiCell
          [notification]="not"
        >
          <ng-container *ngIf="not['package-id'] as pkgId">
            {{ $any(packageData[pkgId])?.manifest.title || pkgId }}
          </ng-container>
          <a
            *ngIf="not['package-id'] && packageData[not['package-id']]"
            tuiButton
            size="xs"
            appearance="secondary"
            [routerLink]="getLink(not['package-id'] || '')"
          >
            View Service
          </a>
        </header-notification>
        <ng-template #blank>
          <div style="padding: 0 1rem">
            Important system alerts and notifications from StartOS will display
            here
          </div>
        </ng-template>
        <a
          style="margin: 1rem; text-align: center; font-size: 0.9rem; font-weight: 600;"
        >
          View all
        </a>
      </tui-scrollbar>
    </ng-container>
  `,
  styles: [
    `
      :host {
        display: flex;
        flex-direction: column;
        height: 100%;
        width: 22rem;
        max-width: 80vw;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    RouterLink,
    TuiForModule,
    TuiScrollbarModule,
    TuiButtonModule,
    HeaderNotificationComponent,
    TuiCellModule,
    TuiAvatarStackModule,
    TuiTitleModule,
  ],
})
export class HeaderNotificationsComponent {
  private readonly patch = inject(PatchDB<DataModel>)
  readonly service = inject(HeaderNotificationsService)

  readonly notifications$ = this.patch
    .watch$('server-info', 'unreadNotifications', 'recent')
    .pipe(first())

  readonly packageData$ = this.patch.watch$('package-data').pipe(first())

  getLink(id: string) {
    return toRouterLink(id)
  }
}
