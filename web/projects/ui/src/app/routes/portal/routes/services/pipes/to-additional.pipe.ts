import { inject, INJECTOR, Pipe, PipeTransform } from '@angular/core'
import { CopyService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import ServiceMarkdownRoute from '../routes/markdown.component'

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
  private readonly copyService = inject(CopyService)
  private readonly markdown = inject(TuiDialogService).open(
    new PolymorpheusComponent(ServiceMarkdownRoute, inject(INJECTOR)),
    { label: 'License', size: 'l' },
  )

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
        name: 'Version',
        description: manifest.version,
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
        action: () => this.markdown.subscribe(),
      },
      {
        name: 'Website',
        description: manifest.marketingSite || FALLBACK_URL,
      },
      {
        name: 'Donation Link',
        description: manifest.donationUrl || FALLBACK_URL,
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
        name: 'Registry',
        description: pkg.registry || FALLBACK_URL,
      },
      {
        name: 'Binary Source',
        description: manifest.wrapperRepo,
      },
    ]
  }
}
