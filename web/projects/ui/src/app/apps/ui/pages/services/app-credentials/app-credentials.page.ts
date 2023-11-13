import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getPkgId, CopyService, ErrorService } from '@start9labs/shared'
import { mask } from 'src/app/util/mask'

@Component({
  selector: 'app-credentials',
  templateUrl: './app-credentials.page.html',
  styleUrls: ['./app-credentials.page.scss'],
})
export class AppCredentialsPage {
  readonly pkgId = getPkgId(this.route)
  credentials: Record<string, string> = {}
  unmasked: { [key: string]: boolean } = {}
  loading = true

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
    private readonly errorService: ErrorService,
    readonly copyService: CopyService,
  ) {}

  async ngOnInit() {
    await this.getCredentials()
  }

  async refresh() {
    await this.getCredentials()
  }

  mask(value: string) {
    return mask(value, 64)
  }

  toggleMask(key: string) {
    this.unmasked[key] = !this.unmasked[key]
  }

  private async getCredentials(): Promise<void> {
    this.loading = true
    try {
      this.credentials = await this.embassyApi.getPackageCredentials({
        id: this.pkgId,
      })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
