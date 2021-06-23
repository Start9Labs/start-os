import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { MarketplaceData, MarketplaceEOS, AvailablePreview } from 'src/app/services/api/api-types'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { ModalController } from '@ionic/angular'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { PackageState } from 'src/app/models/patch-db/data-model'

@Component({
  selector: 'app-available-list',
  templateUrl: './app-available-list.page.html',
  styleUrls: ['./app-available-list.page.scss'],
})
export class AppAvailableListPage {
  pageLoading = true
  pkgsLoading = true
  error = ''

  category = 'featured'
  query: string

  data: MarketplaceData
  eos: MarketplaceEOS
  pkgs: AvailablePreview[] = []

  PackageState = PackageState

  page = 1
  needInfinite = false
  readonly perPage = 20

  constructor (
    private readonly apiService: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly wizardBaker: WizardBaker,
    public patch: PatchDbModel,
  ) { }

  async ngOnInit () {
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

  async doInfinite (e: any): Promise<void> {
    const pkgs = await this.getPkgs()
    this.pkgs = this.pkgs.concat(pkgs)
    e.target.complete()
  }

  async search (e?: any): Promise<void> {
    this.query = e.target.value || undefined
    this.page = 1
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
    this.pkgsLoading = true
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
    this.pkgs = await this.getPkgs()
  }
}
