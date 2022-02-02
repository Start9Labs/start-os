import { Component, ViewChild } from '@angular/core'
import { MarketplacePkg } from 'src/app/services/api/api.types'
import { IonContent } from '@ionic/angular'
import {
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { MarketplaceService } from '../marketplace.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import Fuse from 'fuse.js/dist/fuse.min.js'
import { exists, isEmptyObject } from 'src/app/util/misc.util'
import { filter, first } from 'rxjs/operators'

const defaultOps = {
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
  localPkgs: { [id: string]: PackageDataEntry } = {}
  category = 'featured'
  query: string
  loading = true

  subs: Subscription[] = []

  constructor(
    private readonly errToast: ErrorToastService,
    public readonly patch: PatchDbService,
    public readonly marketplaceService: MarketplaceService,
  ) {}

  async ngOnInit() {
    this.subs = [
      this.patch
        .watch$('package-data')
        .pipe(filter(data => exists(data) && !isEmptyObject(data)))
        .subscribe(pkgs => {
          this.localPkgs = pkgs
          Object.values(this.localPkgs).forEach(pkg => {
            pkg['install-progress'] = { ...pkg['install-progress'] }
          })
        }),
    ]

    this.patch
      .watch$('server-info')
      .pipe(
        filter(data => exists(data) && !isEmptyObject(data)),
        first(),
      )
      .subscribe(async _ => {
        try {
          if (!this.marketplaceService.pkgs.length) {
            await this.marketplaceService.load()
          }

          // category should start as first item in array
          // remove here then add at beginning
          const filterdCategories =
            this.marketplaceService.data.categories.filter(
              cat => this.category !== cat,
            )
          this.categories = [this.category, 'updates']
            .concat(filterdCategories)
            .concat(['all'])

          this.filterPkgs()
        } catch (e) {
          this.errToast.present(e)
        } finally {
          this.loading = false
        }
      })
  }

  ngAfterViewInit() {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy() {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  search(): void {
    if (this.query) {
      this.category = undefined
    }
    this.filterPkgs()
  }

  switchCategory(category: string): void {
    this.category = category
    this.query = undefined
    this.filterPkgs()
  }

  private filterPkgs(): void {
    if (this.category === 'updates') {
      this.pkgs = this.marketplaceService.pkgs.filter(pkg => {
        const { id, version } = pkg.manifest
        return (
          this.localPkgs[id] && version !== this.localPkgs[id].manifest.version
        )
      })
    } else if (this.query) {
      const fuse = new Fuse(this.marketplaceService.pkgs, defaultOps)
      this.pkgs = fuse.search(this.query).map(p => p.item)
    } else {
      const pkgsToSort = this.marketplaceService.pkgs.filter(p => {
        return this.category === 'all' || p.categories.includes(this.category)
      })

      const fuse = new Fuse(pkgsToSort, { ...defaultOps, threshold: 1 })
      this.pkgs = fuse
        .search(this.category !== 'all' ? this.category || '' : 'bit')
        .map(p => p.item)
    }
  }
}
