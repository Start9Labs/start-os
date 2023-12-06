import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  TuiDataListModule,
  TuiDialogService,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { ABOUT } from '../about.component'

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
  private readonly dialogs = inject(TuiDialogService)

  about() {
    this.dialogs.open(ABOUT, { label: 'About this server' }).subscribe()
  }

  logout() {
    this.api.logout({}).catch(e => console.error('Failed to log out', e))
    this.auth.setUnverified()
  }
}
