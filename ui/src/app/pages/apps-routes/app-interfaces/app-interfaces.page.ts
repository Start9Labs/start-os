import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent, ToastController } from '@ionic/angular'
import { Subscription } from 'rxjs'
import { InstalledPackageDataEntry, PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConfigService } from 'src/app/services/config.service'
import { copyToClipboard } from 'src/app/util/web.util'

@Component({
  selector: 'app-Interfaces',
  templateUrl: './app-Interfaces.page.html',
  styleUrls: ['./app-Interfaces.page.scss'],
})
export class AppInterfacesPage {
  pkg: PackageDataEntry

  @ViewChild(IonContent) content: IonContent
  pkgId: string

  constructor (
    private readonly route: ActivatedRoute,
    private readonly toastCtrl: ToastController,
    private readonly config: ConfigService,
    public readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
  }

  ngAfterViewInit () {
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
    })
    await toast.present()
  }

  launch (pkg: PackageDataEntry): void {
    window.open(this.config.launchableURL(pkg), '_blank')
  }

  asIsOrder () {
    return 0
  }
}
