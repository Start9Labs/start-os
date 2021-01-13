import { Injectable } from '@angular/core'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { AppDependency, DependentBreakage, AppInstalledPreview } from '../../models/app-types'
import { ApiService } from '../../services/api/api.service'
import { InstallWizardComponent, SlideDefinition, TopbarParams } from './install-wizard.component'

@Injectable({ providedIn: 'root' })
export class WizardBaker {
  constructor (private readonly apiService: ApiService, private readonly appModel: AppModel) { }

  install (values: {
    id: string, title: string, version: string, serviceRequirements: AppDependency[], developerNotes?: string
  }): InstallWizardComponent['params'] {
    const { id, title, version, serviceRequirements, developerNotes } = values

    validate(id, exists, 'missing id')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')
    validate(serviceRequirements, t => !!t && Array.isArray(t), 'missing serviceRequirements')

    const action = 'install'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      { selector: 'developer-notes', cancelButton: { afterLoading: { text: 'Cancel' } }, nextButton: 'Next', params: {
        action, title, version, serviceRequirements,
      }},
      { selector: 'dependencies', cancelButton: { afterLoading: { text: 'Cancel' } }, nextButton: 'Install', params: {
        action, title, version, serviceRequirements,
      }},
      { selector: 'complete', finishButton: 'Dismiss', cancelButton: { whileLoading: { } }, params: {
        action, verb: 'beginning installation for', title, executeAction: () => this.apiService.installApp(id, version).then(app => {
          this.appModel.add({ ...app, status: AppStatus.INSTALLING })
        }),
      }},
    ]
    return { toolbar, slideDefinitions }
  }

  update (values: {
    id: string, title: string, version: string, serviceRequirements: AppDependency[]
  }): InstallWizardComponent['params'] {
    const { id, title, version, serviceRequirements } = values

    validate(id, exists, 'missing id')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')
    validate(serviceRequirements, t => !!t && Array.isArray(t), 'missing serviceRequirements')

    const action = 'update'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      { selector: 'dependencies', cancelButton: { afterLoading: { text: 'Cancel' } }, nextButton: 'Update', params: {
        action, title, version, serviceRequirements,
      }},
      { selector: 'dependents', cancelButton: { whileLoading: { }, afterLoading: { text: 'Cancel' } }, nextButton: 'Update Anyways', params: {
        skipConfirmationDialogue: true, action, verb: 'updating', title, fetchBreakages: () => this.apiService.installApp(id, version, true).then( ({ breakages }) => breakages ),
      }},
      { selector: 'complete', finishButton: 'Dismiss', cancelButton: { whileLoading: { } }, params: {
        action, verb: 'beginning update for', title, executeAction: () => this.apiService.installApp(id, version).then(app => {
          this.appModel.update({ id: app.id, status: AppStatus.INSTALLING })
        }),
      }},
    ]
    return { toolbar, slideDefinitions }
  }

  downgrade (values: {
    id: string, title: string, version: string, serviceRequirements: AppDependency[]
  }): InstallWizardComponent['params'] {
    const { id, title, version, serviceRequirements } = values

    validate(id, exists, 'missing id')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')
    validate(serviceRequirements, t => !!t && Array.isArray(t), 'missing serviceRequirements')

    const action = 'downgrade'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      { selector: 'dependencies', cancelButton: { afterLoading: { text: 'Cancel' } }, nextButton: 'Downgrade', params: {
        action, title, version, serviceRequirements,
      }},
      { selector: 'dependents', cancelButton: { whileLoading: { }, afterLoading: { text: 'Cancel' } }, nextButton: 'Downgrade Anyways', params: {
        skipConfirmationDialogue: true, action, verb: 'downgrading', title, fetchBreakages: () => this.apiService.installApp(id, version, true).then( ({ breakages }) => breakages ),
      }},
      { selector: 'complete', finishButton: 'Dismiss', cancelButton: { whileLoading: { } }, params: {
        action, verb: 'beginning downgrade for', title, executeAction: () => this.apiService.installApp(id, version).then(app => {
          this.appModel.update({ id: app.id, status: AppStatus.INSTALLING })
        }),
      }},
    ]
    return { toolbar, slideDefinitions }
  }

  uninstall (values: {
    id: string, title: string, version: string
  }): InstallWizardComponent['params'] {
    const { id, title, version } = values

    validate(id, exists, 'missing id')
    validate(title, exists, 'missing title')
    validate(version, exists, 'missing version')

    const action = 'uninstall'
    const toolbar: TopbarParams  = { action, title, version }

    const slideDefinitions: SlideDefinition[] = [
      { selector: 'dependents', cancelButton: { whileLoading: { }, afterLoading: { text: 'Cancel' } }, nextButton: 'Uninstall', params: {
        action, verb: 'uninstalling', title, fetchBreakages: () => this.apiService.uninstallApp(id, true).then( ({ breakages }) => breakages ),
      }},
      { selector: 'complete', finishButton: 'Dismiss', cancelButton: { whileLoading: { } }, params: {
        action, verb: 'uninstalling', title, executeAction: () => this.apiService.uninstallApp(id).then(() => this.appModel.delete(id)),
      }},
    ]
    return { toolbar, slideDefinitions }
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
      { selector: 'dependents', cancelButton: { afterLoading: { text: 'Cancel' } }, nextButton: 'Stop Anyways', params: {
        action, verb: 'stopping', title, fetchBreakages: () => Promise.resolve(breakages),
      }},
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
      { selector: 'dependents', cancelButton: { afterLoading: { text: 'Cancel' } }, nextButton: 'Save Config Anyways', params: {
        action, verb: 'saving config for', title, fetchBreakages: () => Promise.resolve(breakages),
      }},
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

const exists = t => !!t