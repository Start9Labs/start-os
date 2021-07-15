import { Injectable } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Breakages } from 'src/app/services/api/api.types'
import { exists } from 'src/app/util/misc.util'
import { ApiService } from '../../services/api/embassy/embassy-api.service'
import { InstallWizardComponent, SlideDefinition, TopbarParams } from './install-wizard.component'

@Injectable({ providedIn: 'root' })
export class WizardBaker {
  constructor (
    private readonly apiService: ApiService,
  ) { }

  install (values: {
    id: string
    title: string
    version: string
    installAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, installAlert } = values

    const action = 'install'
    const toolbar: TopbarParams = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      installAlert ? {
        slide: {
          selector: 'alert',
          params: {
            title: 'Warning',
            message: installAlert,
            titleColor: 'warning',
          },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } }, next: 'Next',
        },
      } : undefined,
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'beginning installation for',
            title,
            executeAction: () => this.apiService.installPackage({ id, version }),
          },
        },
        bottomBar: {
          cancel: { whileLoading: {  } },
          finish: 'Dismiss',
        },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  update (values: {
    id: string
    title: string
    version: string
    installAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, installAlert } = values

    const action = 'update'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      installAlert ? {
        slide: {
          selector: 'alert',
          params: {
            title: 'Warning',
            message: installAlert,
            titleColor: 'warning',
          },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } },
          next: 'Next',
        },
      } : undefined,
      {
        slide: {
          selector: 'dependents',
          params: {
            skipConfirmationDialogue: true,
            action,
            verb: 'updating',
            title,
            fetchBreakages: () => this.apiService.dryUpdatePackage({ id, version }).then(breakages => breakages),
          },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } },
          next: 'Update Anyways',
        },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'beginning update for',
            title,
            executeAction: () => this.apiService.installPackage({ id, version }),
          },
        },
        bottomBar: {
          cancel: { whileLoading: { } },
          finish: 'Dismiss',
        },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  updateOS (values: {
    version: string
    releaseNotes: { [version: string]: string }
    headline: string
  }): InstallWizardComponent['params'] {
    const { version, releaseNotes, headline } = values

    const action = 'update'
    const title = 'EmbassyOS'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      {
        slide : {
          selector: 'notes',
          params: {
            notes: releaseNotes,
            title: 'Release Notes',
            titleColor: 'dark',
            headline,
          },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } }, next: 'Update OS',
        },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'beginning update for',
            title,
            executeAction: () => this.apiService.updateServer({ }),
          },
        },
        bottomBar: {
          cancel: { whileLoading: { }},
          finish: 'Dismiss',
        },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  downgrade (values: {
    id: string
    title: string
    version: string
    installAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, installAlert } = values

    const action = 'downgrade'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      installAlert ? {
        slide: {
          selector: 'alert',
          params: {
            title: 'Warning',
            message: installAlert,
            titleColor: 'warning',
          },
        },
        bottomBar: { cancel: { afterLoading: { text: 'Cancel' } }, next: 'Next' },
      } : undefined,
      { slide: {
          selector: 'dependents',
          params: {
            skipConfirmationDialogue: true,
            action,
            verb: 'downgrading',
            title,
            fetchBreakages: () => this.apiService.dryUpdatePackage({ id, version }).then(breakages => breakages),
          },
        },
        bottomBar: {
          cancel: { whileLoading: { }, afterLoading: { text: 'Cancel' } }, next: 'Downgrade Anyways',
        },
      },
      { slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'beginning downgrade for',
            title,
            executeAction: () => this.apiService.installPackage({ id, version }),
          },
        },
        bottomBar: {
          cancel: { whileLoading: { } },
          finish: 'Dismiss',
        },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  uninstall (values: {
    id: string
    title: string
    version: string
    uninstallAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, uninstallAlert } = values

    const action = 'uninstall'
    const toolbar: TopbarParams  = { action, title, version }

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
        bottomBar: { cancel: { afterLoading: { text: 'Cancel' } }, next: 'Continue' },
      },
      {
        slide: {
          selector: 'dependents',
          params: {
            action,
            verb: 'uninstalling',
            title,
            fetchBreakages: () => this.apiService.dryRemovePackage({ id }).then(breakages => breakages),
          },
        },
        bottomBar: { cancel: { whileLoading: { }, afterLoading: { text: 'Cancel' } }, next: 'Uninstall' },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'uninstalling',
            title,
            executeAction: () => this.apiService.removePackage({ id }),
          },
        },
        bottomBar: { finish: 'Dismiss', cancel: { whileLoading: { } } },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  stop (values: {
    id: string
    title: string
    version: string
    breakages: Breakages
  }): InstallWizardComponent['params'] {
    const { breakages, title, version } = values

    const action = 'stop'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      {
        slide: {
          selector: 'dependents',
          params: {
            action,
            verb: 'stopping',
            title,
            fetchBreakages: () => Promise.resolve(breakages),
          },
        },
        bottomBar: { cancel: { afterLoading: { text: 'Cancel' } }, next: 'Stop Anyways' },
      },
    ]
    return { toolbar, slideDefinitions }
  }

  configure (values: {
    pkg: PackageDataEntry
    breakages: Breakages
  }): InstallWizardComponent['params'] {
    const { breakages, pkg } = values
    const { title, version } = pkg.manifest
    const action = 'configure'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      {
        slide: {
          selector: 'dependents',
          params: {
            action,
            verb: 'saving config for',
            title, fetchBreakages: () => Promise.resolve(breakages),
          },
        },
        bottomBar: { cancel: { afterLoading: { text: 'Cancel' } }, next: 'Save Config Anyways' },
      },
    ]
    return { toolbar, slideDefinitions }
  }
}

const defaultUninstallWarning = serviceName => `Uninstalling ${ serviceName } will result in the deletion of its data.`
