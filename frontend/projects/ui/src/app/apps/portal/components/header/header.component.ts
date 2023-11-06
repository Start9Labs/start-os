import { ChangeDetectionStrategy, Component } from '@angular/core'
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
import { HeaderMenuComponent } from './header-menu/header-menu.component'

@Component({
  selector: 'header[appHeader]',
  templateUrl: 'header.component.html',
  styleUrls: ['header.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiBadgedContentModule,
    TuiBadgeNotificationModule,
    TuiButtonModule,
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
    HeaderMenuComponent,
  ],
})
export class HeaderComponent {}
