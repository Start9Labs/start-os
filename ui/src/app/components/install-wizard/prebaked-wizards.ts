import { Injectable } from '@angular/core'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { OsUpdateService } from 'src/app/services/os-update.service'
import { exists } from 'src/app/util/misc.util'
import { AppDependency, DependentBreakage, AppInstalledPreview } from '../../models/app-types'
import { ApiService } from '../../services/api/api.service'
import { InstallWizardComponent, SlideDefinition, TopbarParams } from './install-wizard.component'
import { WizardAction } from './wizard-types'

@Injectable({ providedIn: 'root' })
export class WizardBaker {
  constructor (
    private readonly apiService: ApiService,
    private readonly updateService: OsUpdateService,
    private readonly appModel: AppModel,
  ) { }

  install (values: {
    id: string, title: string, version: string, serviceRequirements: AppDependency[], installAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, serviceRequirements, installAlert } = values

    validate(id, exists, 'missing id')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')
    validate(serviceRequirements, t => !!t && Array.isArray(t), 'missing serviceRequirements')

    const action = 'install'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      installAlert ? {
        slide: {
          selector: 'notes',
          params: { notes: installAlert, title: 'Warning', titleColor: 'warning' },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } }, next: 'Next',
        },
      } : undefined,
      {
        slide: {
          selector: 'dependencies',
          params: { action, title, version, serviceRequirements },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } },
          next: 'Install',
        },
      },
      {
        slide: {
          selector: 'complete',
          params: {
            action,
            verb: 'beginning installation for',
            title,
            executeAction: () => this.apiService.installApp(id, version).then(app => { this.appModel.add({ ...app, status: AppStatus.INSTALLING })}),
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
    id: string, title: string, version: string, serviceRequirements: AppDependency[], installAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, serviceRequirements, installAlert } = values

    validate(id, exists, 'missing id')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')
    validate(serviceRequirements, t => !!t && Array.isArray(t), 'missing serviceRequirements')

    const action = 'update'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      installAlert ? {
        slide: {
          selector: 'notes',
          params: { notes: installAlert, title: 'Warning', titleColor: 'warning'},
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } },
          next: 'Next',
        },
      } : undefined,
      { slide: {
          selector: 'dependencies',
          params: { action, title, version, serviceRequirements },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } },
          next: 'Update',
        },
      },
      { slide: {
          selector: 'dependents',
          params: {
            skipConfirmationDialogue: true, action, verb: 'updating', title, fetchBreakages: () => this.apiService.installApp(id, version, true).then( ({ breakages }) => breakages ),
          },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } },
          next: 'Update Anyways',
        },
      },
      { slide: {
          selector: 'complete',
          params: {
            action, verb: 'beginning update for', title, executeAction: () => this.apiService.installApp(id, version).then(app => {
              this.appModel.update({ id: app.id, status: AppStatus.INSTALLING })
            }),
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
    version: string, releaseNotes: string
  }): InstallWizardComponent['params'] {
    const { version, releaseNotes } = values

    const action = 'update'
    const title = 'EmbassyOS'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      { slide : {
          selector: 'notes',
          params: { notes: releaseNotes, title: 'Release Notes', titleColor: 'dark' },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } }, next: 'Update OS',
        },
      },
      { slide: {
          selector: 'complete',
          params: {
            action, verb: 'beginning update for', title, executeAction: () => this.updateService.updateEmbassyOS(version),
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
    id: string, title: string, version: string, serviceRequirements: AppDependency[], installAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, serviceRequirements, installAlert } = values

    validate(id, exists, 'missing id')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')
    validate(serviceRequirements, t => !!t && Array.isArray(t), 'missing serviceRequirements')

    const action = 'downgrade'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      installAlert ? {
        slide: {
          selector: 'notes',
          params: { notes: installAlert, title: 'Warning', titleColor: 'warning' },
        },
        bottomBar: { cancel: { afterLoading: { text: 'Cancel' } }, next: 'Next' },
      } : undefined,
      { slide: {
          selector: 'dependencies',
          params: { action, title, version, serviceRequirements },
        },
        bottomBar: {
          cancel: { afterLoading: { text: 'Cancel' } },
          next: 'Downgrade',
        },
      },
      { slide: {
          selector: 'dependents',
          params: {
            skipConfirmationDialogue: true, action, verb: 'downgrading', title, fetchBreakages: () => this.apiService.installApp(id, version, true).then( ({ breakages }) => breakages ),
          },
        },
        bottomBar: {
          cancel: { whileLoading: { }, afterLoading: { text: 'Cancel' } }, next: 'Downgrade Anyways',
        },
      },
      { slide: {
          selector: 'complete',
          params: {
            action, verb: 'beginning downgrade for', title, executeAction: () => this.apiService.installApp(id, version).then(app => {
              this.appModel.update({ id: app.id, status: AppStatus.INSTALLING })
            }),
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
    id: string, title: string, version: string, uninstallAlert?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, uninstallAlert } = values

    validate(id, exists, 'missing id')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')

    const action = 'uninstall' as WizardAction
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      { slide: {
          selector: 'notes',
          params: {
            notes: uninstallAlert || defaultUninstallationWarning(title),
            title: 'Warning',
            titleColor: 'warning',
          },
        },
        bottomBar: { cancel: { afterLoading: { text: 'Cancel' } }, next: 'Continue' },
      },
      { slide: {
          selector: 'dependents',
          params: {
            action, verb: 'uninstalling', title, fetchBreakages: () => this.apiService.uninstallApp(id, true).then( ({ breakages }) => breakages ),
          },
        },
        bottomBar: { cancel: { whileLoading: { }, afterLoading: { text: 'Cancel' } }, next: 'Uninstall' },
      },
      { slide: {
          selector: 'complete',
          params: {
            action, verb: 'uninstalling', title, executeAction: () => this.apiService.uninstallApp(id).then(() => this.appModel.delete(id)),
          },
        },
        bottomBar: { finish: 'Dismiss', cancel: { whileLoading: { } } },
      },
    ]
    return { toolbar, slideDefinitions: slideDefinitions.filter(exists) }
  }

  stop (values: {
    breakages: DependentBreakage[], id: string, title: string, version: string
  }): InstallWizardComponent['params'] {
    const { breakages, title, version } = values

    validate(breakages, t => !!t && Array.isArray(t), 'missing breakages')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')

    const action = 'stop'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      { slide: {
          selector: 'dependents',
          params: {
            action, verb: 'stopping', title, fetchBreakages: () => Promise.resolve(breakages),
          },
        },
        bottomBar: { cancel: { afterLoading: { text: 'Cancel' } }, next: 'Stop Anyways' },
      },
    ]
    return { toolbar, slideDefinitions }
  }

  configure (values: {
    breakages: DependentBreakage[], app: AppInstalledPreview
  }): InstallWizardComponent['params'] {
    const { breakages, app } = values
    const { title, versionInstalled: version  } = app
    const action = 'configure'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      { slide: {
          selector: 'dependents',
          params: {
            action, verb: 'saving config for', title, fetchBreakages: () => Promise.resolve(breakages),
          },
        },
        bottomBar: { cancel: { afterLoading: { text: 'Cancel' } }, next: 'Save Config Anyways' },
      },
    ]
    return { toolbar, slideDefinitions }
  }
}

function validate<T> (t: T, test: (t: T) => Boolean, desc: string) {
  if (!test(t)) {
    console.error('failed validation', desc, t)
    throw new Error(desc)
  }
}


const defaultUninstallationWarning = serviceName => `Uninstalling ${ serviceName } will result in the deletion of its data.`
