import { inject, Pipe, PipeTransform } from '@angular/core'
import { CopyService, MARKDOWN } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiDialogService } from '@taiga-ui/core'
import { from } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

export const FALLBACK_URL = 'Not provided'

export interface AdditionalItem {
  name: string
  description: string
  icon?: string
  action?: () => void
}

@Pipe({
  name: 'toAdditional',
  standalone: true,
})
export class ToAdditionalPipe implements PipeTransform {
  private readonly api = inject(ApiService)
  private readonly copyService = inject(CopyService)
  private readonly dialogs = inject(TuiDialogService)

  transform(pkg: PackageDataEntry): AdditionalItem[] {
    const manifest = getManifest(pkg)
    return [
      {
        name: 'Installed',
        description: new Intl.DateTimeFormat('en-US', {
          dateStyle: 'medium',
          timeStyle: 'medium',
        }).format(new Date(pkg.installedAt || 0)),
      },
      {
        name: 'Git Hash',
        description: manifest.gitHash || 'Unknown',
        icon: manifest.gitHash ? '@tui.copy' : '',
        action: () =>
          manifest.gitHash && this.copyService.copy(manifest.gitHash),
      },
      {
        name: 'License',
        description: manifest.license,
        icon: '@tui.chevron-right',
        action: () => this.showLicense(manifest),
      },
      {
        name: 'Website',
        description: manifest.marketingSite || FALLBACK_URL,
      },
      {
        name: 'Source Repository',
        description: manifest.upstreamRepo,
      },
      {
        name: 'Support Site',
        description: manifest.supportSite || FALLBACK_URL,
      },
      {
        name: 'Donation Link',
        description: manifest.donationUrl || FALLBACK_URL,
      },
    ]
  }

  private showLicense({ id, version }: T.Manifest) {
    this.dialogs
      .open(MARKDOWN, {
        label: 'License',
        size: 'l',
        data: {
          content: from(this.api.getStaticInstalled(id, 'LICENSE.md')),
        },
      })
      .subscribe()
  }
}
