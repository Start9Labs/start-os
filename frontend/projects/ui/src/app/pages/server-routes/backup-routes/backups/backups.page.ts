import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'backups',
  templateUrl: './backups.page.html',
  styleUrls: ['./backups.page.scss'],
})
export class BackupsPage {
  readonly secure = this.config.isSecure()

  constructor(private readonly config: ConfigService) {}
}
