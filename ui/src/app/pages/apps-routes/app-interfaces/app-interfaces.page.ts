import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent, ToastController } from '@ionic/angular'
import { Subscription } from 'rxjs'
import { InstalledPackageDataEntry, PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
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
  subs: Subscription[] = []

  constructor (
    private readonly route: ActivatedRoute,
    private readonly toastCtrl: ToastController,
    private readonly config: ConfigService,
    private readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    const pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.subs = [
      this.patch.watch$('package-data', pkgId).subscribe(pkg => this.pkg = pkg),
    ]
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
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
