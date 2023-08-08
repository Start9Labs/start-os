import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiBadgedContentModule } from '@taiga-ui/kit'
import {
  TuiButtonModule,
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'

@Component({
  selector: 'header[appHeader]',
  templateUrl: 'header.component.html',
  styleUrls: ['header.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiBadgedContentModule,
    TuiButtonModule,
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
  ],
})
export class HeaderComponent {}
