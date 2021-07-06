import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { MarketplaceData, MarketplaceEOS, AvailablePreview } from 'src/app/services/api/api-types'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { ModalController } from '@ionic/angular'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry, PackageState } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
})
export class MarketplaceListPage {
  pageLoading = true
  pkgsLoading = true
  error = ''

  category = 'featured'
  query: string

  data: MarketplaceData
  eos: MarketplaceEOS
  pkgs: AvailablePreview[] = []
  installedPkgs: { [id: string]: PackageDataEntry } = { }

  PackageState = PackageState

  page = 1
  needInfinite = false
  readonly perPage = 20

  subs: Subscription[] = []

  constructor (
    private readonly apiService: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly wizardBaker: WizardBaker,
    private readonly patch: PatchDbModel,
  ) { }

  async ngOnInit () {
    this.subs = [
      this.patch.watch$('package-data')
      .subscribe(pkgs => this.installedPkgs = pkgs),
    ]

    try {
      const [data, eos, pkgs] = await Promise.all([
        this.apiService.getMarketplaceData({ }),
        this.apiService.getEos({ }),
        this.getPkgs(),
      ])
      this.data = data
      this.eos = eos
      this.pkgs = pkgs
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      this.pageLoading = false
      this.pkgsLoading = false
    }
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async doInfinite (e: any): Promise<void> {
    const pkgs = await this.getPkgs()
    this.pkgs = this.pkgs.concat(pkgs)
    e.target.complete()
  }

  async search (e?: any): Promise<void> {
    this.query = e.target.value || undefined
    this.page = 1
    this.pkgsLoading = true
    this.pkgs = await this.getPkgs()
  }

  async updateEos (): Promise<void> {
    await wizardModal(
      this.modalCtrl,
      this.wizardBaker.updateOS({
        version: this.eos.version,
        releaseNotes: this.eos['release-notes'],
      }),
    )
  }

  private async getPkgs (): Promise<AvailablePreview[]> {
    try {
      const pkgs = await this.apiService.getAvailableList({
        category: this.category,
        query: this.query,
        page: this.page,
        'per-page': this.perPage,
      })
      this.needInfinite = pkgs.length >= this.perPage
      this.page++
      return pkgs
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      this.pkgsLoading = false
    }
  }

  async switchCategory (category: string): Promise<void> {
    this.category = category
    this.pkgsLoading = true
    this.page = 1
    this.pkgs = await this.getPkgs()
  }
}
