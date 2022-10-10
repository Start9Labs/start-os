import { Component, Inject } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import {
  MarketplaceService,
  RequestStatus,
} from 'src/app/services/marketplace.service'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { Emver } from '@start9labs/shared'
import { Pipe, PipeTransform } from '@angular/core'

@Component({
  selector: 'updates',
  templateUrl: 'updates.page.html',
  styleUrls: ['updates.page.scss'],
})
export class UpdatesPage {
  readonly hosts$ = this.patch.watch$('ui', 'marketplace', 'known-hosts')
  readonly updateRequests$ = this.marketplaceService.getUpdateRequests$()
  readonly localPkgs$ = this.patch.watch$('package-data')
  readonly marketplaceCache$ = this.marketplaceService.getCache$()

  RequestStatus = RequestStatus

  constructor(
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async update(id: string, url: string): Promise<void> {
    this.api.installPackage({ id, 'marketplace-url': url })
  }
}

@Pipe({
  name: 'filterUpdates',
})
export class FilterUpdatesPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(
    pkgs: MarketplacePkg[],
    local: Record<string, PackageDataEntry> = {},
  ): MarketplacePkg[] {
    return pkgs.filter(
      ({ manifest }) =>
        local[manifest.id] &&
        this.emver.compare(
          manifest.version,
          local[manifest.id].installed?.manifest.version || '',
        ) === 1,
    )
  }
}
