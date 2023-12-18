import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  inject,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { Manifest } from '@start9labs/marketplace'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiSvgModule } from '@taiga-ui/core'
import { TuiLineClampModule } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { Observable, first } from 'rxjs'
import { ServerNotification } from 'src/app/services/api/api.types'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { NotificationService } from '../../../services/notification.service'
import { toRouterLink } from '../../../utils/to-router-link'

@Component({
  selector: '[notificationItem]',
  template: `
    <td><ng-content /></td>
    <td>{{ notificationItem['created-at'] | date : 'MMM d, y, h:mm a' }}</td>
    <td [style.color]="color">
      <tui-svg [style.color]="color" [src]="icon"></tui-svg>
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
      <a *ngIf="overflow" (click)="service.viewFull(notificationItem)">
        View Full
      </a>
      <a
        *ngIf="notificationItem.code === 1"
        (click)="service.viewReport(notificationItem)"
      >
        View Report
      </a>
    </td>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, RouterLink, TuiLineClampModule, TuiSvgModule],
})
export class NotificationItemComponent {
  private readonly patch = inject(PatchDB<DataModel>)
  readonly service = inject(NotificationService)

  @Input({ required: true }) notificationItem!: ServerNotification<number>

  overflow = false

  @tuiPure
  get manifest$(): Observable<Manifest> {
    return this.patch
      .watch$(
        'package-data',
        this.notificationItem['package-id'] || '',
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