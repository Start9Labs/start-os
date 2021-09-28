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
    } else {
      this.pkgs = this.marketplaceService.pkgs.filter(pkg => {
        const { id, title, description } = pkg.manifest
        if (this.query) {
          const query = this.query.toUpperCase()
          return  id.toUpperCase().includes(query) ||
                  title.toUpperCase().includes(query) ||
                  description.short.toUpperCase().includes(query) ||
                  description.long.toUpperCase().includes(query)
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
}
