import { inject, Pipe, PipeTransform } from '@angular/core'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { Manifest } from '@start9labs/marketplace'
import { CopyService, MarkdownComponent } from '@start9labs/shared'
import { from } from 'rxjs'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'

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

  transform({ manifest, installed }: PackageDataEntry): AdditionalItem[] {
    return [
      {
        name: 'Installed',
        description: new Intl.DateTimeFormat('en-US', {
          dateStyle: 'medium',
          timeStyle: 'medium',
        }).format(new Date(installed?.['installed-at'] || 0)),
      },
      {
        name: 'Git Hash',
        description: manifest['git-hash'] || 'Unknown',
        icon: manifest['git-hash'] ? 'tuiIconCopyLarge' : '',
        action: () =>
          manifest['git-hash'] && this.copyService.copy(manifest['git-hash']),
      },
      {
        name: 'License',
        description: manifest.license,
        icon: 'tuiIconChevronRightLarge',
        action: () => this.showLicense(manifest),
      },
      {
        name: 'Website',
        description: manifest['marketing-site'] || FALLBACK_URL,
      },
      {
        name: 'Source Repository',
        description: manifest['upstream-repo'],
      },
      {
        name: 'Support Site',
        description: manifest['support-site'] || FALLBACK_URL,
      },
      {
        name: 'Donation Link',
        description: manifest['donation-url'] || FALLBACK_URL,
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
