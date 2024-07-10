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
import { TuiLinkModule } from '@taiga-ui/core'
import { TuiIconModule } from '@taiga-ui/experimental'
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
    <td class="checkbox"><ng-content /></td>
    <td class="date">
      {{ notificationItem.createdAt | date: 'medium' }}
    </td>
    <td class="title" [style.color]="color">
      <tui-icon [icon]="icon" [style.font-size.rem]="1" />
      {{ notificationItem.title }}
    </td>
    <td class="service">
      @if (manifest$ | async; as manifest) {
        <a tuiLink [routerLink]="getLink(manifest.id)">
          {{ manifest.title }}
        </a>
      } @else {
        N/A
      }
    </td>
    <td class="content">
      <tui-line-clamp
        style="pointer-events: none"
        [linesLimit]="4"
        [lineHeight]="21"
        [content]="notificationItem.message"
        (overflownChange)="overflow = $event"
      />
      @if (overflow) {
        <button tuiLink (click)="service.viewFull(notificationItem)">
          View Full
        </button>
      }
      @if (notificationItem.code === 1) {
        <button tuiLink (click)="service.viewReport(notificationItem)">
          View Report
        </button>
      }
    </td>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  host: {
    '[class._new]': '!notificationItem.read',
  },
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    :host {
      grid-template-columns: 1fr;

      &._new {
        background: var(--tui-clear) !important;
      }
    }

    button {
      position: relative;
    }

    td {
      padding: 0.25rem;
      vertical-align: top;
    }

    .checkbox {
      padding-top: 0.4rem;
    }

    :host-context(tui-root._mobile) {
      .checkbox {
        @include fullsize();
      }

      .date {
        order: 1;
        color: var(--tui-text-02);
      }

      .title {
        font-weight: bold;
        font-size: 1.2em;
        display: flex;
        align-items: center;
        gap: 0.75rem;
      }

      .service:not(:has(a)) {
        display: none;
      }
    }
  `,
  imports: [
    CommonModule,
    RouterLink,
    TuiLineClampModule,
    TuiLinkModule,
    TuiIconModule,
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
