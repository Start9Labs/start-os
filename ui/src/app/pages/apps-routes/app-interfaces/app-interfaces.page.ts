import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent, ToastController } from '@ionic/angular'
import { InstalledPackageDataEntry } from 'src/app/models/patch-db/data-model'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { ConfigService } from 'src/app/services/config.service'
import { copyToClipboard } from 'src/app/util/web.util'

@Component({
  selector: 'app-Interfaces',
  templateUrl: './app-Interfaces.page.html',
  styleUrls: ['./app-Interfaces.page.scss'],
})
export class AppInterfacesPage {
  pkgId: string

  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly route: ActivatedRoute,
    private readonly toastCtrl: ToastController,
    private readonly config: ConfigService,
    public readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
  }

  async ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  async copy (address: string): Promise<void> {
    let message = ''
    await copyToClipboard(address || '')
      .then(success => { message = success ? 'copied to clipboard!' : 'failed to copy'})

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
      cssClass: 'notification-toast',
    })
    await toast.present()
  }

  launch (installed: InstalledPackageDataEntry): void {
    window.open(this.config.launchableURL(installed), '_blank')
  }

  asIsOrder () {
    return 0
  }
}
