import { Component, ViewChild } from '@angular/core'
import { MarketplaceData, MarketplaceEOS, MarketplacePkg } from 'src/app/services/api/api.types'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { IonContent, ModalController } from '@ionic/angular'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageState } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { MarketplaceService } from '../marketplace.service'
import { MarketplaceApiService } from 'src/app/services/api/marketplace/marketplace-api.service'

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
})
export class MarketplaceListPage {
  @ViewChild(IonContent) content: IonContent
  pageLoading = true
  pkgsLoading = true

  category = 'all'
  query: string

  data: MarketplaceData
  eos: MarketplaceEOS
  pkgs: MarketplacePkg[] = []

  PackageState = PackageState

  page = 1
  needInfinite = false
  readonly perPage = 30

  subs: Subscription[] = []

  constructor (
    private readonly marketplaceService: MarketplaceService,
    private readonly marketplaceApiService: MarketplaceApiService,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly wizardBaker: WizardBaker,
    public readonly patch: PatchDbService,
  ) { }

  async ngOnInit () {

    try {
      const [data, eos, pkgs] = await Promise.all([
        this.marketplaceApiService.getMarketplaceData({ }),
        this.marketplaceApiService.getEos({ }),
        this.getPkgs(),
      ])
      this.data = data
      this.data.categories.unshift(this.category)
      this.eos = eos
      this.pkgs = pkgs
    } catch (e) {
      console.error(e)
      this.errToast.present(e.message)
    } finally {
      this.pageLoading = false
      this.pkgsLoading = false
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
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
        headline: this.eos.headline,
        releaseNotes: this.eos['release-notes'],
      }),
    )
  }

  private async getPkgs (): Promise<MarketplacePkg[]> {
    try {
      const pkgs = await this.marketplaceService.getPkgs(
        this.category !== 'all' ? this.category : undefined,
        this.query,
        this.page,
        this.perPage,
      )
      this.needInfinite = pkgs.length >= this.perPage
      this.page++
      return pkgs
    } catch (e) {
      console.error(e)
      this.errToast.present(e.message)
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
