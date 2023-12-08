import { CommonModule } from '@angular/common'
import { Router } from '@angular/router'
import { TuiSidebarModule } from '@taiga-ui/addon-mobile'
import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  HostListener,
  inject,
  ViewChild,
} from '@angular/core'
import { tuiContainsOrAfter, tuiIsElement, TuiLetModule } from '@taiga-ui/cdk'
import {
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import {
  TuiBadgedContentModule,
  TuiBadgeNotificationModule,
  TuiButtonModule,
} from '@taiga-ui/experimental'
import { Subject } from 'rxjs'
import { SidebarDirective } from '../../../../app/sidebar-host.component'
import { HeaderMenuComponent } from './header-menu.component'
import { HeaderNotificationsComponent } from './header-notifications.component'
import { NotificationService } from '../../services/notification.service'

@Component({
  selector: 'header[appHeader]',
  template: `
    <ng-content></ng-content>
    <button
      tuiIconButton
      iconLeft="tuiIconCloudLarge"
      appearance="success"
      [style.margin-left]="'auto'"
    >
      Connection
    </button>
    <tui-badged-content
      *tuiLet="notificationService.unreadCount$ | async as unread"
      [style.--tui-radius.%]="50"
    >
      <tui-badge-notification *ngIf="unread" tuiSlot="bottom" size="s">
        {{ unread }}
      </tui-badge-notification>
      <button
        tuiIconButton
        iconLeft="tuiIconBellLarge"
        appearance="warning"
        (click)="handleNotificationsClick(unread || 0)"
      >
        Notifications
      </button>
    </tui-badged-content>
    <header-menu></header-menu>
    <header-notifications
      (onEmpty)="this.open$.next(false)"
      *tuiSidebar="!!(open$ | async); direction: 'right'; autoWidth: true"
    />
  `,
  styles: [
    `
      :host {
        display: flex;
        align-items: center;
        height: 4.5rem;
        padding: 0 1rem 0 2rem;
        font-size: 1.5rem;
        // TODO: Theme
        background: rgb(51 51 51 / 84%);
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiBadgedContentModule,
    TuiBadgeNotificationModule,
    TuiButtonModule,
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
    TuiSidebarModule,
    SidebarDirective,
    HeaderMenuComponent,
    HeaderNotificationsComponent,
    TuiLetModule,
  ],
})
export class HeaderComponent {
  private readonly router = inject(Router)
  readonly notificationService = inject(NotificationService)

  @ViewChild(HeaderNotificationsComponent, { read: ElementRef })
  private readonly panel?: ElementRef<HTMLElement>

  private readonly _ = this.router.events.subscribe(() => {
    this.open$.next(false)
  })

  readonly open$ = new Subject<boolean>()

  @HostListener('document:click.capture', ['$event.target'])
  onClick(target: EventTarget | null) {
    if (
      tuiIsElement(target) &&
      this.panel?.nativeElement &&
      !tuiContainsOrAfter(this.panel.nativeElement, target)
    ) {
      this.open$.next(false)
    }
  }

  handleNotificationsClick(unread: number) {
    if (unread) {
      this.open$.next(true)
    } else {
      this.router.navigateByUrl('/portal/system/notifications')
    }
  }
}
