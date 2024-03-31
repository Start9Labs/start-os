import { inject, Pipe, PipeTransform } from '@angular/core'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { CopyService, MarkdownComponent } from '@start9labs/shared'
import { from } from 'rxjs'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getManifest } from 'src/app/util/get-package-data'
import { Manifest } from '../../../../../../../../../../core/startos/bindings/Manifest'

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
        icon: manifest.gitHash ? 'tuiIconCopyLarge' : '',
        action: () =>
          manifest.gitHash && this.copyService.copy(manifest.gitHash),
      },
      {
        name: 'License',
        description: manifest.license,
        icon: 'tuiIconChevronRightLarge',
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

  private showLicense({ id, version }: Manifest) {
    this.dialogs
      .open(new PolymorpheusComponent(MarkdownComponent), {
        label: 'License',
        size: 'l',
        data: {
          content: from(
            this.api.getStatic(
              `/public/package-data/${id}/${version}/LICENSE.md`,
            ),
          ),
        },
      })
      .subscribe()
  }
}
