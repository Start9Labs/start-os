import { Component, Inject, Optional, Type, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import {
  LIST_HEADER_CONTENT,
  LOAD_TRIGGER,
  LOCAL_PACKAGES,
  LocalPackages,
  MarketplacePkg,
  AbstractMarketplaceService,
} from '@start9labs/marketplace'
import { Observable, Subscription } from 'rxjs'
import { ErrorToastService, PackageState } from '@start9labs/shared'
import Fuse from 'fuse.js/dist/fuse.min.js'
import { Manifest } from 'src/app/services/patch-db/data-model'

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

// TODO: Refactor
type Package = MarketplacePkg & { manifest: Manifest }

@Component({
  selector: 'marketplace-list',
  templateUrl: './marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
})
export class MarketplaceListPage {
  PackageState = PackageState

  @ViewChild(IonContent) content: IonContent

  pkgs: Package[] = []
  categories: string[]
  localPkgs: LocalPackages = {}
  category = 'featured'
  query: string
  loading = true

  subs: Subscription[] = []

  constructor(
    private readonly errToast: ErrorToastService,
    public readonly marketplaceService: AbstractMarketplaceService,
    @Inject(LOCAL_PACKAGES)
    private readonly localPkgs$: Observable<LocalPackages>,
    @Inject(LOAD_TRIGGER)
    private readonly loadTrigger$: Observable<unknown>,
    @Optional()
    @Inject(LIST_HEADER_CONTENT)
    readonly headerContent: Type<any> | null,
  ) {}

  async ngOnInit() {
    this.subs = [
      this.localPkgs$.subscribe(pkgs => {
        this.localPkgs = pkgs
      }),

      this.loadTrigger$.subscribe(async _ => {
        try {
          if (!this.marketplaceService.pkgs.length) {
            await this.marketplaceService.load()
          }

          // category should start as first item in array
          // remove here then add at beginning
          const filteredCategories =
            this.marketplaceService.data.categories.filter(
              cat => this.category !== cat,
            )
          this.categories = [this.category, 'updates']
            .concat(filteredCategories)
            .concat(['all'])

          this.filterPkgs()
        } catch (e) {
          this.errToast.present(e)
        } finally {
          this.loading = false
        }
      }),
    ]
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
      // TODO: Fix type
      this.pkgs = this.marketplaceService.pkgs.filter(pkg => {
        const { id, version } = pkg.manifest
        return (
          this.localPkgs[id] && version !== this.localPkgs[id].manifest.version
        )
      }) as Package[]
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
