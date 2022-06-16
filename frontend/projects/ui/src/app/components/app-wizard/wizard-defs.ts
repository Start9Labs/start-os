import { Inject, Injectable } from '@angular/core'
import { exists } from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { Manifest } from 'src/app/services/patch-db/data-model'
import { ApiService } from '../../services/api/embassy-api.service'
import { AppWizardComponent, SlideDefinition } from './app-wizard.component'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { first } from 'rxjs/operators'

@Injectable({ providedIn: 'root' })
export class WizardDefs {
  constructor(
    private readonly embassyApi: ApiService,
    private readonly config: ConfigService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
  ) {}

  update(values: {
    id: string
    title: string
    version: string
    installAlert?: string
  }): AppWizardComponent['params'] {
    const { id, title, version, installAlert } = values

    const slides: Array<SlideDefinition | undefined> = [
      installAlert
        ? {
            selector: 'alert',
            params: {
              message: installAlert,
            },
          }
        : undefined,
      {
        selector: 'complete',
        params: {
          verb: 'beginning update for',
          title,
          Fn: () =>
            this.marketplaceService
              .installPackage({
                id,
                'version-spec': version ? `=${version}` : undefined,
              })
              .pipe(first())
              .toPromise(),
        },
      },
    ]
    return {
      action: 'update',
      title,
      version,
      slides: slides.filter(exists),
      submitBtn: 'Begin Update',
    }
  }

  downgrade(values: {
    id: string
    title: string
    version: string
    installAlert?: string
  }): AppWizardComponent['params'] {
    const { id, title, version, installAlert } = values

    const slides: Array<SlideDefinition | undefined> = [
      installAlert
        ? {
            selector: 'alert',
            params: {
              message: installAlert,
            },
          }
        : undefined,
      {
        selector: 'complete',
        params: {
          verb: 'beginning downgrade for',
          title,
          Fn: () =>
            this.marketplaceService
              .installPackage({
                id,
                'version-spec': version ? `=${version}` : undefined,
              })
              .pipe(first())
              .toPromise(),
        },
      },
    ]

    return {
      action: 'downgrade',
      title,
      version,
      slides: slides.filter(exists),
      submitBtn: 'Begin Downgrade',
    }
  }

  uninstall(values: {
    id: string
    title: string
    uninstallAlert?: string
  }): AppWizardComponent['params'] {
    const { id, title, uninstallAlert } = values

    const slides: SlideDefinition[] = [
      {
        selector: 'alert',
        params: {
          message: uninstallAlert || defaultUninstallWarning(title),
        },
      },
      {
        selector: 'complete',
        params: {
          verb: 'uninstalling',
          title,
          Fn: () => this.embassyApi.uninstallPackage({ id }),
        },
      },
    ]

    return {
      action: 'uninstall',
      title,
      slides: slides.filter(exists),
      submitBtn: 'Uninstall Anyway',
    }
  }

  stop(values: { id: string; title: string }): AppWizardComponent['params'] {
    const { title, id } = values

    const slides: SlideDefinition[] = [
      {
        selector: 'complete',
        params: {
          verb: 'stopping',
          title,
          Fn: () => this.embassyApi.stopPackage({ id }),
        },
      },
    ]

    return {
      action: 'stop',
      title,
      slides: slides.filter(exists),
      submitBtn: 'Stop Anyway',
    }
  }

  configure(values: {
    manifest: Manifest
    config: object
  }): AppWizardComponent['params'] {
    const { manifest, config } = values
    const { id, title } = manifest

    const slides: SlideDefinition[] = [
      {
        selector: 'dependents',
        params: {
          verb: 'saving config for',
          title,
          Fn: () => this.embassyApi.drySetPackageConfig({ id, config }),
        },
      },
      {
        selector: 'complete',
        params: {
          verb: 'configuring',
          title,
          Fn: () => this.embassyApi.setPackageConfig({ id, config }),
        },
      },
    ]

    return {
      action: 'configure',
      title,
      slides: slides.filter(exists),
      submitBtn: 'Configure Anyway',
    }
  }
}

const defaultUninstallWarning = (serviceName: string) =>
  `Uninstalling ${serviceName} will result in the deletion of its data.`
