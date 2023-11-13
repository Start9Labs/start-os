import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { EOSService } from 'src/app/services/eos.service'

@Component({
  selector: 'os-update',
  templateUrl: './os-update.page.html',
  styleUrls: ['./os-update.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class OSUpdatePage {
  versions: { version: string; notes: string }[] = []

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT) private readonly context: TuiDialogContext,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
    private readonly eosService: EOSService,
  ) {}

  ngOnInit() {
    const releaseNotes = this.eosService.eos?.['release-notes']!

    this.versions = Object.keys(releaseNotes)
      .sort()
      .reverse()
      .map(version => ({
        version,
        notes: releaseNotes[version],
      }))
  }

  async updateEOS() {
    const loader = this.loader.open('Beginning update...').subscribe()

    try {
      await this.embassyApi.updateServer()
      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
