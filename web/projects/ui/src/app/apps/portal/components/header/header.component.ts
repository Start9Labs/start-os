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
import { tuiContainsOrAfter, tuiIsElement } from '@taiga-ui/cdk'
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
import { ToNotificationsPipe } from '../../pipes/to-notifications'
import { HeaderMenuComponent } from './header-menu.component'
import { HeaderNotificationsComponent } from './header-notifications.component'

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
    <tui-badged-content [style.--tui-radius.%]="50">
      <tui-badge-notification
        *ngIf="'notifications' | toNotifications | async as unread"
        tuiSlot="bottom"
        size="s"
      >
        {{ unread }}
      </tui-badge-notification>
      <button
        tuiIconButton
        iconLeft="tuiIconBellLarge"
        appearance="warning"
        (click)="open$.next(true)"
      >
        Notifications
      </button>
    </tui-badged-content>
    <header-menu></header-menu>
    <header-notifications *tuiSidebar="!!(open$ | async); direction: 'right'" />
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
    ToNotificationsPipe,
  ],
})
export class HeaderComponent {
  @ViewChild(HeaderNotificationsComponent, { read: ElementRef })
  private readonly panel?: ElementRef<HTMLElement>

  private readonly navigation = inject(Router).events.subscribe(() => {
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
}
