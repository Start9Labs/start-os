import { Component, ViewChild } from '@angular/core'
import { MarketplaceData, MarketplaceEOS, MarketplacePkg } from 'src/app/services/api/api.types'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { IonContent, ModalController } from '@ionic/angular'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { PackageDataEntry, PackageState } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { MarketplaceService } from '../marketplace.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { pauseFor } from 'src/app/util/misc.util'

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
})
export class MarketplaceListPage {
  @ViewChild(IonContent) content: IonContent
  localPkgs: { [id: string]: PackageDataEntry } = { }

  pageLoading = true
  pkgsLoading = true

  category = 'featured'
  query: string

  data: MarketplaceData
  eos: MarketplaceEOS
  pkgs: MarketplacePkg[] = []

  PackageState = PackageState

  subs: Subscription[] = []

  constructor (
    private readonly marketplaceService: MarketplaceService,
    private readonly api: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly wizardBaker: WizardBaker,
    public readonly patch: PatchDbService,
  ) { }

  async ngOnInit () {
    this.subs = [
      this.patch.watch$('package-data').subscribe(pkgs => {
        this.localPkgs = pkgs
        Object.values(this.localPkgs).forEach(pkg => {
          pkg['install-progress'] = { ...pkg['install-progress'] }
        })
      }),
    ]

    try {
      const [data, eos] = await Promise.all([
        this.api.getMarketplaceData({ }),
        this.api.getEos({ }),
        this.marketplaceService.getAllPkgs(),
      ])
      this.eos = eos
      this.data = data

      this.pkgsLoading = false
      this.getPkgs()

      // category should start as first item in array
      // remove here then add at beginning
      const filterdCategories = this.data.categories.filter(cat => this.category !== cat)
      this.data.categories = [this.category, 'updates'].concat(filterdCategories).concat(['all'])

    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.pageLoading = false
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async search (): Promise<void> {
    // you can actually press enter and run code before the data binds to this.category
    await pauseFor(200)
    this.category = undefined
    await this.getPkgs()
  }

  async updateEos (): Promise<void> {
    await wizardModal(
      this.modalCtrl,
      this.wizardBaker.updateOS({
        version: this.eos.version,
        headline: this.eos.headline,
        releaseNotes: this.eos['release-notes'],
      }),
    )
  }

  private async getPkgs (): Promise<void> {
    if (this.category === 'updates') {
      this.pkgs = this.marketplaceService.allPkgs.filter(pkg => {
        return this.localPkgs[pkg.manifest.id] && pkg.manifest.version !== this.localPkgs[pkg.manifest.id].manifest.version
      })
    } else {
      this.pkgs = this.marketplaceService.allPkgs.filter(pkg => {
        if (this.query) {
          return  pkg.manifest.id.toUpperCase().includes(this.query.toUpperCase()) ||
                  pkg.manifest.title.toUpperCase().includes(this.query.toUpperCase()) ||
                  pkg.manifest.description.short.toUpperCase().includes(this.query.toUpperCase()) ||
                  pkg.manifest.description.long.toUpperCase().includes(this.query.toUpperCase())
        } else {
          if (this.category === 'all' || !this.category) {
            return true
          } else {
            return pkg.categories.includes(this.category)
          }
        }
      })
    }
  }

  async switchCategory (category: string): Promise<void> {
    this.category = category
    this.query = undefined
    await this.getPkgs()
  }
}
