import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  AlertController,
  IonBackButtonDelegate,
  ModalController,
  NavController,
  ToastController,
} from '@ionic/angular'
import { PackageProperties } from 'src/app/util/properties.util'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'
import {
  DestroyService,
  ErrorToastService,
  getPkgId,
  copyToClipboard,
} from '@start9labs/shared'
import { getValueByPointer } from 'fast-json-patch'
import { map, takeUntil } from 'rxjs/operators'

@Component({
  selector: 'app-properties',
  templateUrl: './app-properties.page.html',
  styleUrls: ['./app-properties.page.scss'],
  providers: [DestroyService],
})
export class AppPropertiesPage {
  loading = true
  readonly pkgId = getPkgId(this.route)

  pointer = ''
  node: PackageProperties = {}

  properties: PackageProperties = {}
  unmasked: { [key: string]: boolean } = {}

  stopped$ = this.patch
    .watch$('package-data', this.pkgId, 'installed', 'status', 'main', 'status')
    .pipe(map(status => status === PackageMainStatus.Stopped))

  @ViewChild(IonBackButtonDelegate, { static: false })
  backButton?: IonBackButtonDelegate

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly toastCtrl: ToastController,
    private readonly modalCtrl: ModalController,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
    private readonly destroy$: DestroyService,
  ) {}

  ionViewDidEnter() {
    if (!this.backButton) return
    this.backButton.onClick = () => {
      history.back()
    }
  }

  async ngOnInit() {
    await this.getProperties()

    this.route.queryParams
      .pipe(takeUntil(this.destroy$))
      .subscribe(queryParams => {
        if (queryParams['pointer'] === this.pointer) return
        this.pointer = queryParams['pointer'] || ''
        this.node = getValueByPointer(this.properties, this.pointer)
      })
  }

  async refresh() {
    await this.getProperties()
  }

  async presentDescription(
    property: { key: string; value: PackageProperties[''] },
    e: Event,
  ) {
    e.stopPropagation()

    const alert = await this.alertCtrl.create({
      header: property.key,
      message: property.value.description || undefined,
    })
    await alert.present()
  }

  async goToNested(key: string): Promise<any> {
    this.navCtrl.navigateForward(`/services/${this.pkgId}/properties`, {
      queryParams: {
        pointer: `${this.pointer}/${key}/value`,
      },
    })
  }

  async copy(text: string): Promise<void> {
    let message = ''
    await copyToClipboard(text).then(success => {
      message = success
        ? 'Copied to clipboard!'
        : 'Failed to copy to clipboard.'
    })

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  async showQR(text: string): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: QRComponent,
      componentProps: {
        text,
      },
      cssClass: 'qr-modal',
    })
    await modal.present()
  }

  toggleMask(key: string) {
    this.unmasked[key] = !this.unmasked[key]
  }

  private async getProperties(): Promise<void> {
    this.loading = true
    try {
      this.properties = await this.embassyApi.getPackageProperties({
        id: this.pkgId,
      })
      this.node = getValueByPointer(this.properties, this.pointer)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
