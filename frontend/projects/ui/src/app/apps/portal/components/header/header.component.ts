import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiBadgedContentModule } from '@taiga-ui/kit'
import { TuiButtonModule } from '@taiga-ui/core'

@Component({
  selector: 'header[appHeader]',
  templateUrl: 'header.component.html',
  styleUrls: ['header.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiBadgedContentModule, TuiButtonModule],
})
export class HeaderComponent {}
