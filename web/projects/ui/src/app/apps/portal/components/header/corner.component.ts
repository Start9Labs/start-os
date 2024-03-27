import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  HostListener,
  inject,
  ViewChild,
} from '@angular/core'
import { Router } from '@angular/router'
import { TuiSidebarModule } from '@taiga-ui/addon-mobile'
import { tuiContainsOrAfter, tuiIsElement, TuiLetModule } from '@taiga-ui/cdk'
import {
  TuiBadgedContentModule,
  TuiBadgeNotificationModule,
  TuiButtonModule,
} from '@taiga-ui/experimental'
import { Subject } from 'rxjs'
import { HeaderMenuComponent } from './menu.component'
import { HeaderNotificationsComponent } from './notifications.component'
import { SidebarDirective } from 'src/app/common/sidebar-host.component'
import { NotificationService } from '../../services/notification.service'

@Component({
  standalone: true,
  selector: 'header-corner',
  template: `
    <ng-content />
    <tui-badged-content
      *tuiLet="notificationService.unreadCount$ | async as unread"
      [style.--tui-radius.%]="50"
    >
      <tui-badge-notification *ngIf="unread" tuiSlot="top" size="s">
        {{ unread }}
      </tui-badge-notification>
      <button
        tuiIconButton
        iconLeft="tuiIconBellLarge"
        appearance="icon"
        size="s"
        [style.color]="'var(--tui-text-01)'"
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
        gap: 0.25rem;
        padding: 0 0.5rem 0 1.75rem;
        --clip-path: polygon(0% 0%, 100% 0%, 100% 100%, 1.75rem 100%);
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    HeaderMenuComponent,
    HeaderNotificationsComponent,
    SidebarDirective,
    TuiBadgeNotificationModule,
    TuiBadgedContentModule,
    TuiButtonModule,
    TuiLetModule,
    TuiSidebarModule,
  ],
})
export class HeaderCornerComponent {
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
