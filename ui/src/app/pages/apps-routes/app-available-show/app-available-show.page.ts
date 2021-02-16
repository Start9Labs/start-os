import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/api.service'
import { AlertController, ModalController, NavController, PopoverController } from '@ionic/angular'
import { Recommendation } from 'src/app/components/recommendation-button/recommendation-button.component'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { InformationPopoverComponent } from 'src/app/components/information-popover/information-popover.component'
import { Emver } from 'src/app/services/emver.service'
import { displayEmver } from 'src/app/pipes/emver.pipe'
import { pauseFor } from 'src/app/util/misc.util'
import { AvailableShow } from 'src/app/services/api/api-types'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { PackageState } from 'src/app/models/patch-db/data-model'

@Component({
  selector: 'app-available-show',
  templateUrl: './app-available-show.page.html',
  styleUrls: ['./app-available-show.page.scss'],
})
export class AppAvailableShowPage {
  loading = true
  error = ''
  pkg: AvailableShow
  pkgId: string

  PackageState = PackageState

  rec: Recommendation | null = null
  showRec = true

  depDefinition = '<span style="font-style: italic">Service Dependencies</span> are other services that this service recommends or requires in order to run.'

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly wizardBaker: WizardBaker,
    private readonly navCtrl: NavController,
    private readonly popoverController: PopoverController,
    private readonly emver: Emver,
    public readonly patch: PatchDbModel,
  ) { }

  async ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId') as string
    this.rec = history.state && history.state.installRec as Recommendation
    this.getPkg()
  }

  async getPkg (version?: string): Promise<void> {
    this.loading = true
    try {
      this.pkg = await this.apiService.getAvailableShow({ id: this.pkgId, version })
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      this.loading = false
    }
  }

  async presentPopover (information: string, ev: any) {
    const popover = await this.popoverController.create({
      component: InformationPopoverComponent,
      event: ev,
      translucent: false,
      showBackdrop: true,
      backdropDismiss: true,
      componentProps: {
        information,
      },
    })
    return await popover.present()
  }

  async presentAlertVersions () {
    const alert = await this.alertCtrl.create({
      header: 'Versions',
      backdropDismiss: false,
      inputs: this.pkg.versions.sort((a, b) => -1 * this.emver.compare(a, b)).map(v => {
        return {
          name: v, // for CSS
          type: 'radio',
          label: displayEmver(v), // appearance on screen
          value: v, // literal SEM version value
          checked: this.pkg.manifest.version === v,
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
    const { id, title, version, dependencies, alerts } = this.pkg.manifest
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
    const { id, title, version, dependencies, alerts } = this.pkg.manifest
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
