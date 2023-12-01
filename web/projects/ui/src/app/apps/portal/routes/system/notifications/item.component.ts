import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
  inject,
} from '@angular/core'
import { ServerNotification } from 'src/app/services/api/api.types'
import { TuiForModule } from '@taiga-ui/cdk'
import { TuiCheckboxModule, TuiLineClampModule } from '@taiga-ui/kit'
import { FormsModule } from '@angular/forms'
import { NotificationService } from '../../../services/notification.service'
import { TuiButtonModule, TuiSvgModule } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { Observable, first } from 'rxjs'
import { toRouterLink } from '../../../utils/to-router-link'
import { RouterLink } from '@angular/router'
import { Manifest } from '@start9labs/marketplace'

@Component({
  selector: 'notification-item',
  template: `
    <td>
      <input
        tuiCheckbox
        size="s"
        type="checkbox"
        [ngModel]="selected"
        (ngModelChange)="onToggle.emit(notification)"
      />
    </td>
    <td [class.unread]="!notification.read">
      {{ notification['created-at'] | date : 'MMM d, y, h:mm a' }}
    </td>
    <td [class.unread]="!notification.read" [style.color]="color">
      <tui-svg
        style="align-self: flex-start; margin: 0.2rem 0;"
        [style.color]="color"
        [src]="icon"
      ></tui-svg>
      {{ notification.title }}
    </td>
    <td [class.unread]="!notification.read">
      <a
        *ngIf="manifest$ | async as manifest; else na"
        [routerLink]="getLink(manifest.id)"
      >
        {{ manifest.title }}
      </a>
      <ng-template #na>N/A</ng-template>
    </td>
    <td [class.unread]="!notification.read">
      <tui-line-clamp
        style="pointer-events: none"
        [linesLimit]="4"
        [lineHeight]="21"
        [content]="notification.message"
        (overflownChange)="overflow = $event"
      />
      <div style="padding-bottom: .5rem;">
        <a *ngIf="overflow" (click)="service.viewFull(notification)">
          View Full
        </a>
        <a
          *ngIf="notification.code === 1"
          (click)="service.viewReport(notification)"
        >
          View Report
        </a>
      </div>
    </td>
  `,
  styles: [
    `
      :host {
        display: contents;
      }

      .unread {
        font-weight: 800;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    RouterLink,
    TuiForModule,
    TuiCheckboxModule,
    FormsModule,
    TuiLineClampModule,
    TuiButtonModule,
    TuiSvgModule,
  ],
})
export class NotificationItemComponent {
  readonly service = inject(NotificationService)
  private readonly patch = inject(PatchDB<DataModel>)

  @Input({ required: true }) selected!: boolean
  @Input({ required: true }) notification!: ServerNotification<number>

  @Output() onToggle = new EventEmitter<ServerNotification<number>>()

  overflow = false

  get manifest$(): Observable<Manifest> {
    return this.patch
      .watch$('package-data', this.notification['package-id'] || '', 'manifest')
      .pipe(first())
  }

  get color(): string {
    return this.service.getColor(this.notification)
  }

  get icon(): string {
    return this.service.getIcon(this.notification)
  }

  getLink(id: string) {
    return toRouterLink(id)
  }
}
