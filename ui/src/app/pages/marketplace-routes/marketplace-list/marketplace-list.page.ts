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

  page = 1
  needInfinite = false
  readonly perPage = 30

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
      }),
    ]

    try {
      const [data, eos] = await Promise.all([
        this.api.getMarketplaceData({ }),
        this.api.getEos({ }),
        this.getPkgs(),
      ])
      this.eos = eos
      this.data = data

      // category should start as first item in array
      // remove here then add at beginning
      const filterdCategories = this.data.categories.filter(cat => this.category !== cat)
      this.data.categories = [this.category, 'updates'].concat(filterdCategories).concat(['all'])

    } catch (e) {
      this.errToast.present(e)
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
    await this.getPkgs(true)
    e.target.complete()
  }

  async search (): Promise<void> {
    if (!this.query) return
    this.pkgsLoading = true
    this.category = undefined
    this.page = 1
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

  private async getPkgs (doInfinite = false): Promise<void> {
    try {
      if (this.category === 'updates') {
        this.pkgs = this.marketplaceService.updates
        if (this.pkgs.length) {
          this.pkgsLoading = false
        }
        await this.marketplaceService.getUpdates(this.localPkgs)
        this.pkgs = this.marketplaceService.updates
      } else {
        const pkgs = await this.marketplaceService.getPkgs(
          this.category !== 'all' ? this.category : undefined,
          this.query,
          this.page,
          this.perPage,
        )
        this.needInfinite = pkgs.length >= this.perPage
        this.page++
        this.pkgs = doInfinite ? this.pkgs.concat(pkgs) : pkgs
      }
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.pkgsLoading = false
    }
  }

  async switchCategory (category: string): Promise<void> {
    this.pkgsLoading = true
    this.category = category
    this.query = undefined
    this.page = 1
    await this.getPkgs()
  }
}
