import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Output,
  inject,
  EventEmitter,
} from '@angular/core'
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
import { Subject, first, tap } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderNotificationComponent } from './notification.component'
import { toRouterLink } from '../../utils/to-router-link'
import {
  ServerNotification,
  ServerNotifications,
} from 'src/app/services/api/api.types'
import { NotificationService } from '../../services/notification.service'

@Component({
  selector: 'header-notifications',
  template: `
    <ng-container *ngIf="notifications$ | async as notifications">
      <h3 class="g-title" style="padding: 0 1rem">
        Notifications
        <a
          *ngIf="notifications.length"
          style="margin-left: auto; text-transform: none; font-size: 0.9rem; font-weight: 600;"
          (click)="markAllSeen(notifications[0].id)"
        >
          Mark All Seen
        </a>
      </h3>
      <tui-scrollbar *ngIf="packageData$ | async as packageData">
        <header-notification
          *ngFor="let not of notifications; let i = index"
          tuiCell
          [notification]="not"
        >
          <ng-container *ngIf="not['package-id'] as pkgId">
            {{ $any(packageData[pkgId])?.manifest.title || pkgId }}
          </ng-container>
          <button
            style="align-self: flex-start; flex-shrink: 0;"
            tuiIconButton
            appearance="icon"
            iconLeft="tuiIconMinusCircle"
            (click)="markSeen(notifications, not)"
          ></button>
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
      </tui-scrollbar>
      <a
        style="margin: 2rem; text-align: center; font-size: 0.9rem; font-weight: 600;"
        [routerLink]="'/portal/system/notifications'"
      >
        View All
      </a>
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
  private readonly service = inject(NotificationService)

  readonly packageData$ = this.patch.watch$('package-data').pipe(first())

  readonly notifications$ = new Subject<ServerNotifications>()

  @Output() onEmpty = new EventEmitter()

  ngAfterViewInit() {
    this.patch
      .watch$('server-info', 'unreadNotifications', 'recent')
      .pipe(
        tap(recent => this.notifications$.next(recent)),
        first(),
      )
      .subscribe()
  }

  markSeen(
    current: ServerNotifications,
    notification: ServerNotification<number>,
  ) {
    this.notifications$.next(current.filter(c => c.id !== notification.id))

    if (current.length === 1) this.onEmpty.emit()

    this.service.markSeen([notification])
  }

  markAllSeen(latestId: number) {
    this.notifications$.next([])

    this.service.markSeenAll(latestId)

    this.onEmpty.emit()
  }

  getLink(id: string) {
    return toRouterLink(id)
  }
}
