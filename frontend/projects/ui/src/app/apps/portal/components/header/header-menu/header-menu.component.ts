import { ChangeDetectionStrategy, Component } from '@angular/core'
import {
  TuiButtonModule,
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'

@Component({
  selector: 'header-menu',
  templateUrl: 'header-menu.component.html',
  styleUrls: ['header-menu.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
    TuiButtonModule,
  ],
})
export class HeaderMenuComponent {}
