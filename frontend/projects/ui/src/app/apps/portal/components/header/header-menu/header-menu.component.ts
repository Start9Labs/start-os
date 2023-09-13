import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  TuiButtonModule,
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'

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
export class HeaderMenuComponent {
  private readonly api = inject(ApiService)
  private readonly auth = inject(AuthService)

  logout() {
    this.api.logout({}).catch(e => console.error('Failed to log out', e))
    this.auth.setUnverified()
  }
}
