import { Component, ViewChild } from '@angular/core'
import { MarketplacePkg } from 'src/app/services/api/api.types'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { IonContent, ModalController } from '@ionic/angular'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { PackageDataEntry, PackageState } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { MarketplaceService } from '../marketplace.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import Fuse from 'fuse.js/dist/fuse.min.js'

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
})
export class MarketplaceListPage {
  PackageState = PackageState

  @ViewChild(IonContent) content: IonContent

  pkgs: MarketplacePkg[] = []
  categories: string[]
  localPkgs: { [id: string]: PackageDataEntry } = { }
  category = 'featured'
  query: string
  loading = true

  subs: Subscription[] = []

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly wizardBaker: WizardBaker,
    private readonly patch: PatchDbService,
    public readonly marketplaceService: MarketplaceService,
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
      if (!this.marketplaceService.pkgs.length) {
        await this.marketplaceService.load()
      }

      // category should start as first item in array
      // remove here then add at beginning
      const filterdCategories = this.marketplaceService.data.categories.filter(cat => this.category !== cat)
      this.categories = [this.category, 'updates'].concat(filterdCategories).concat(['all'])

      this.filterPkgs()

    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async search (): Promise<void> {
    this.category = undefined
    await this.filterPkgs()
  }

  async switchCategory (category: string): Promise<void> {
    this.category = category
    this.query = undefined
    this.filterPkgs()
  }

  async updateEos (): Promise<void> {
    const { version, headline, 'release-notes': releaseNotes } = this.marketplaceService.eos

    await wizardModal(
      this.modalCtrl,
      this.wizardBaker.updateOS({
        version,
        headline,
        releaseNotes,
      }),
    )
  }

  private async filterPkgs (): Promise<void> {
    if (this.category === 'updates') {
      this.pkgs = this.marketplaceService.pkgs.filter(pkg => {
        const { id, version } = pkg.manifest
        return this.localPkgs[id] && version !== this.localPkgs[id].manifest.version
      })
    } else if (this.query) {
      const options = {
        isCaseSensitive: false,
        includeScore: true,
        shouldSort: true,
        includeMatches: false,
        findAllMatches: false,
        minMatchCharLength: 1,
        location: 0,
        threshold: 0.6,
        distance: 100,
        useExtendedSearch: false,
        ignoreLocation: false,
        ignoreFieldNorm: false,
        keys: [
          'manifest.id',
          'manifest.title',
          'manifest.description.short',
          'manifest.description.long',
        ],
      }
      const fuse = new Fuse(this.marketplaceService.pkgs, options)
      this.pkgs = fuse.search(this.query).map(p => p.item)

    } else {
      const options = {
        isCaseSensitive: false,
        includeScore: true,
        shouldSort: true,
        includeMatches: false,
        findAllMatches: false,
        minMatchCharLength: 1,
        location: 0,
        threshold: 1,
        distance: 100,
        useExtendedSearch: false,
        ignoreLocation: false,
        ignoreFieldNorm: false,
        keys: [
          'manifest.id',
          'manifest.title',
          'manifest.description.short',
          'manifest.description.long',
        ],
      }


      const pkgsToSort = this.marketplaceService.pkgs.filter(p => {
        if (this.category === 'all') return true
        return p.categories.includes(this.category)
      })

      const fuse = new Fuse(pkgsToSort, options)
      this.pkgs = fuse.search(this.category !== 'all' ? this.category : 'bit').map(p => p.item)
    }
  }
}
