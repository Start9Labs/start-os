import { Component, ViewChild } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConfigService } from 'src/app/services/config.service'
import { IonContent } from '@ionic/angular'

@Component({
  selector: 'security-options',
  templateUrl: './security-options.page.html',
  styleUrls: ['./security-options.page.scss'],
})
export class SecurityOptionsPage {
  @ViewChild(IonContent) content: IonContent

  constructor (
    public readonly serverConfig: ServerConfigService,
    public readonly config: ConfigService,
    public readonly patch: PatchDbService,
  ) { }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }
}
