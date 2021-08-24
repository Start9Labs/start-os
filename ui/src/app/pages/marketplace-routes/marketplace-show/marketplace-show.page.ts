import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AlertController, IonContent, ModalController, NavController } from '@ionic/angular'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { Emver } from 'src/app/services/emver.service'
import { displayEmver } from 'src/app/pipes/emver.pipe'
import { pauseFor, Recommendation } from 'src/app/util/misc.util'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { PackageDataEntry, PackageState } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from '../marketplace.service'
import { Subscription } from 'rxjs'
import { MarkdownPage } from 'src/app/modals/markdown/markdown.page'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
})
export class MarketplaceShowPage {
  @ViewChild(IonContent) content: IonContent
  loading = true
  pkgId: string
  localPkg: PackageDataEntry
  PackageState = PackageState
  rec: Recommendation | null = null
  showRec = true

  subs: Subscription[] = []

  constructor (
    private readonly route: ActivatedRoute,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly wizardBaker: WizardBaker,
    private readonly navCtrl: NavController,
    private readonly emver: Emver,
    private readonly patch: PatchDbService,
    private readonly marketplaceService: MarketplaceService,
  ) { }

  async ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.rec = history.state && history.state.installRec as Recommendation

    this.subs = [
      this.patch.watch$('package-data', this.pkgId)
      .subscribe(pkg => {
        this.localPkg = pkg
      }),
    ]

    if (this.marketplaceService.pkgs[this.pkgId]) {
      this.loading = false
    } else {
      this.getPkg()
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async getPkg (version?: string): Promise<void> {
    try {
      await this.marketplaceService.getPkg(this.pkgId, version)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      await pauseFor(100)
      this.loading = false
    }
  }

  async presentAlertVersions () {
    const alert = await this.alertCtrl.create({
      header: 'Versions',
      inputs: this.marketplaceService.pkgs[this.pkgId].versions.sort((a, b) => -1 * this.emver.compare(a, b)).map(v => {
        return {
          name: v, // for CSS
          type: 'radio',
          label: displayEmver(v), // appearance on screen
          value: v, // literal SEM version value
          checked: this.marketplaceService.pkgs[this.pkgId].manifest.version === v,
        }
      }),
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        }, {
          text: 'Ok',
          handler: (version: string) => {
            this.getPkg(version)
          },
          cssClass: 'enter-click',
        },
      ],
    })

    await alert.present()
  }

  async presentModalMd (title: string) {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title,
        contentUrl: this.marketplaceService.pkgs[this.pkgId][title],
      },
      component: MarkdownPage,
    })

    await modal.present()
  }

  async install () {
    const { id, title, version, alerts } = this.marketplaceService.pkgs[this.pkgId].manifest
    const { cancelled } = await wizardModal(
      this.modalCtrl,
      this.wizardBaker.install({
        id,
        title,
        version,
        installAlert: alerts.install,
      }),
    )
    if (cancelled) return
  }

  async update (action: 'update' | 'downgrade') {
    const { id, title, version, dependencies, alerts } = this.marketplaceService.pkgs[this.pkgId].manifest
    const value = {
      id,
      title,
      version,
      serviceRequirements: dependencies,
      installAlert: alerts.install,
    }

    const { cancelled } = await wizardModal(
      this.modalCtrl,
      action === 'update' ?
        this.wizardBaker.update(value) :
        this.wizardBaker.downgrade(value),
    )

    if (cancelled) return
    await pauseFor(250)
    this.navCtrl.back()
  }

  dismissRec () {
    this.showRec = false
  }
}
