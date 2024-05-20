import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { T } from '@start9labs/start-sdk'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiLinkModule, TuiSvgModule } from '@taiga-ui/core'
import { TuiLineClampModule } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { first, Observable } from 'rxjs'
import { ServerNotification } from 'src/app/services/api/api.types'
import { NotificationService } from 'src/app/services/notification.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { toRouterLink } from 'src/app/utils/to-router-link'

@Component({
  selector: '[notificationItem]',
  template: `
    <td [style.padding-top.rem]="0.4"><ng-content /></td>
    <td>{{ notificationItem.createdAt | date: 'MMM d, y, h:mm a' }}</td>
    <td [style.color]="color">
      <tui-svg [src]="icon" />
      {{ notificationItem.title }}
    </td>
    <td>
      <a
        *ngIf="manifest$ | async as manifest; else na"
        [routerLink]="getLink(manifest.id)"
      >
        {{ manifest.title }}
      </a>
      <ng-template #na>N/A</ng-template>
    </td>
    <td [style.padding-bottom.rem]="0.5">
      <tui-line-clamp
        style="pointer-events: none"
        [linesLimit]="4"
        [lineHeight]="21"
        [content]="notificationItem.message"
        (overflownChange)="overflow = $event"
      />
      <button
        *ngIf="overflow"
        tuiLink
        (click)="service.viewFull(notificationItem)"
      >
        View Full
      </button>
      <button
        *ngIf="notificationItem.code === 1"
        tuiLink
        (click)="service.viewReport(notificationItem)"
      >
        View Report
      </button>
    </td>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  host: {
    '[class._new]': '!notificationItem.read',
  },
  styles: `
    :host._new {
      background: var(--tui-clear);
    }

    td {
      padding: 0.25rem;
      vertical-align: top;
    }
  `,
  imports: [
    CommonModule,
    RouterLink,
    TuiLineClampModule,
    TuiSvgModule,
    TuiLinkModule,
  ],
})
export class NotificationItemComponent {
  private readonly patch = inject(PatchDB<DataModel>)
  readonly service = inject(NotificationService)

  @Input({ required: true }) notificationItem!: ServerNotification<number>

  overflow = false

  @tuiPure
  get manifest$(): Observable<T.Manifest> {
    return this.patch
      .watch$(
        'packageData',
        this.notificationItem.packageId || '',
        'stateInfo',
        'manifest',
      )
      .pipe(first())
  }

  get color(): string {
    return this.service.getColor(this.notificationItem)
  }

  get icon(): string {
    return this.service.getIcon(this.notificationItem)
  }

  getLink(id: string) {
    return toRouterLink(id)
  }
}
