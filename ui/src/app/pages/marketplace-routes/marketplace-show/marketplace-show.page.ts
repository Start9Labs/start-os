import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { Recommendation } from 'src/app/components/recommendation-button/recommendation-button.component'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { Emver } from 'src/app/services/emver.service'
import { displayEmver } from 'src/app/pipes/emver.pipe'
import { pauseFor } from 'src/app/util/misc.util'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { PackageState } from 'src/app/models/patch-db/data-model'
import { MarketplaceService } from '../marketplace.service'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
})
export class MarketplaceShowPage {
  error = ''
  pkgId: string

  PackageState = PackageState

  rec: Recommendation | null = null
  showRec = true

  constructor (
    private readonly route: ActivatedRoute,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly wizardBaker: WizardBaker,
    private readonly navCtrl: NavController,
    private readonly emver: Emver,
    public readonly patch: PatchDbModel,
    public marketplaceService: MarketplaceService,
  ) { }

  async ngOnInit () {
    this.rec = history.state && history.state.installRec as Recommendation
    this.getPkg()
  }

  async getPkg (version?: string): Promise<void> {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    try {
      await this.marketplaceService.setPkg(this.pkgId, version)
    } catch (e) {
      console.error(e)
      this.error = e.message
    }
  }

  async presentAlertVersions () {
    const alert = await this.alertCtrl.create({
      header: 'Versions',
      backdropDismiss: false,
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
        },
      ],
    })

    await alert.present()
  }

  async install () {
    const { id, title, version, dependencies, alerts } = this.marketplaceService.pkgs[this.pkgId].manifest
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
    await pauseFor(250)
    this.navCtrl.back()
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
