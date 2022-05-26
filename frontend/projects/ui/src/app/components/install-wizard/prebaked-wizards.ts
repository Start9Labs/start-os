import { Inject, Injectable } from '@angular/core'
import { exists } from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Breakages } from 'src/app/services/api/api.types'
import { ApiService } from '../../services/api/embassy-api.service'
import {
  InstallWizardComponent,
  SlideDefinition,
  TopbarParams,
} from './install-wizard.component'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { first } from 'rxjs/operators'

@Injectable({ providedIn: 'root' })
export class WizardBaker {
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
  }): InstallWizardComponent['params'] {
    const { id, title, version, installAlert } = values

    const action = 'update'
    const toolbar: TopbarParams = { action, title, version }

    const slideDefinitions: Array<SlideDefinition | undefined> = [
      installAlert
        ? {
            slide: {
              selector: 'alert',
              params: {
                title: 'Warning',
                message: installAlert,
                titleColor: 'warning',
              },
            },
            bottomBar: {
              cancel: {
                afterLoading: { text: 'Cancel' },
              },
              next: 'Next',
            },
          }
        : undefined,
      {
        slide: {
          selector: 'dependents',
          params: {
            action,
            verb: 'updating',
            title,
            fetchBreakages: () =>
              this.embassyApi
                .dryUpdatePackage({ id, version })
                .then(breakages => breakages),
          },
        },
        bottomBar: {
          cancel: {
            afterLoading: { text: 'Cancel' },
          },
          next: 'Update Anyway',
        },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'beginning update for',
            title,
            executeAction: () =>
              this.marketplaceService
                .installPackage({
                  id,
                  'version-spec': version ? `=${version}` : undefined,
                })
                .pipe(first())
                .toPromise(),
          },
        },
        bottomBar: {
          cancel: { whileLoading: {} },
          finish: 'Dismiss',
        },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  updateOS(values: {
    version: string
    releaseNotes: { [version: string]: string }
    headline: string
  }): InstallWizardComponent['params'] {
    const { version, releaseNotes, headline } = values

    const versions = Object.keys(releaseNotes)
      .sort()
      .reverse()
      .map(version => {
        return {
          version,
          notes: releaseNotes[version],
        }
      })

    const action = 'update'
    const title = 'EmbassyOS'
    const toolbar: TopbarParams = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      {
        slide: {
          selector: 'notes',
          params: {
            versions,
            title: 'Release Notes',
            titleColor: 'dark',
            headline,
          },
        },
        bottomBar: {
          cancel: {
            afterLoading: { text: 'Cancel' },
          },
          next: 'Begin Update',
        },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'beginning update for',
            title,
            executeAction: () =>
              this.embassyApi.updateServer({
                'marketplace-url': this.config.marketplace.url,
              }),
          },
        },
        bottomBar: {
          cancel: { whileLoading: {} },
          finish: 'Dismiss',
        },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  downgrade(values: {
    id: string
    title: string
    version: string
    installAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, installAlert } = values

    const action = 'downgrade'
    const toolbar: TopbarParams = { action, title, version }

    const slideDefinitions: Array<SlideDefinition | undefined> = [
      installAlert
        ? {
            slide: {
              selector: 'alert',
              params: {
                title: 'Warning',
                message: installAlert,
                titleColor: 'warning',
              },
            },
            bottomBar: {
              cancel: {
                afterLoading: { text: 'Cancel' },
              },
              next: 'Next',
            },
          }
        : undefined,
      {
        slide: {
          selector: 'dependents',
          params: {
            action,
            verb: 'downgrading',
            title,
            fetchBreakages: () =>
              this.embassyApi
                .dryUpdatePackage({ id, version })
                .then(breakages => breakages),
          },
        },
        bottomBar: {
          cancel: {
            whileLoading: {},
            afterLoading: { text: 'Cancel' },
          },
          next: 'Downgrade Anyway',
        },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'beginning downgrade for',
            title,
            executeAction: () =>
              this.marketplaceService
                .installPackage({
                  id,
                  'version-spec': version ? `=${version}` : undefined,
                })
                .pipe(first())
                .toPromise(),
          },
        },
        bottomBar: {
          cancel: { whileLoading: {} },
          finish: 'Dismiss',
        },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  uninstall(values: {
    id: string
    title: string
    version: string
    uninstallAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, uninstallAlert } = values

    const action = 'uninstall'
    const toolbar: TopbarParams = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      {
        slide: {
          selector: 'alert',
          params: {
            title: 'Warning',
            message: uninstallAlert || defaultUninstallWarning(title),
            titleColor: 'warning',
          },
        },
        bottomBar: {
          cancel: {
            afterLoading: { text: 'Cancel' },
          },
          next: 'Continue',
        },
      },
      {
        slide: {
          selector: 'dependents',
          params: {
            action,
            verb: 'uninstalling',
            title,
            fetchBreakages: () =>
              this.embassyApi
                .dryUninstallPackage({ id })
                .then(breakages => breakages),
          },
        },
        bottomBar: {
          cancel: {
            whileLoading: {},
            afterLoading: { text: 'Cancel' },
          },
          next: 'Uninstall',
        },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'uninstalling',
            title,
            executeAction: () => this.embassyApi.uninstallPackage({ id }),
          },
        },
        bottomBar: {
          finish: 'Dismiss',
          cancel: {
            whileLoading: {},
          },
        },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  stop(values: {
    id: string
    title: string
    version: string
  }): InstallWizardComponent['params'] {
    const { title, version, id } = values

    const action = 'stop'
    const toolbar: TopbarParams = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      {
        slide: {
          selector: 'dependents',
          params: {
            action,
            verb: 'stopping',
            title,
            fetchBreakages: () =>
              this.embassyApi
                .dryStopPackage({ id })
                .then(breakages => breakages),
          },
        },
        bottomBar: {
          cancel: {
            whileLoading: {},
            afterLoading: { text: 'Cancel' },
          },
          next: 'Stop Anyway',
        },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'stopping',
            title,
            executeAction: () => this.embassyApi.stopPackage({ id }),
          },
        },
        bottomBar: {
          finish: 'Dismiss',
          cancel: {
            whileLoading: {},
          },
        },
      },
    ]
    return { toolbar, slideDefinitions }
  }

  configure(values: {
    pkg: PackageDataEntry
    breakages: Breakages
  }): InstallWizardComponent['params'] {
    const { breakages, pkg } = values
    const { title, version } = pkg.manifest
    const action = 'configure'
    const toolbar: TopbarParams = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      {
        slide: {
          selector: 'dependents',
          params: {
            action,
            verb: 'saving config for',
            title,
            fetchBreakages: () => Promise.resolve(breakages),
          },
        },
        bottomBar: {
          cancel: {
            afterLoading: { text: 'Cancel' },
          },
          next: 'Save Config Anyway',
        },
      },
    ]
    return { toolbar, slideDefinitions }
  }
}

const defaultUninstallWarning = (serviceName: string) =>
  `Uninstalling ${serviceName} will result in the deletion of its data.`
