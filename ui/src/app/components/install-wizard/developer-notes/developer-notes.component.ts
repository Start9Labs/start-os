import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, from, Subject } from 'rxjs'
import { takeUntil } from 'rxjs/operators'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { capitalizeFirstLetter } from 'src/app/util/misc.util'
import { Colorable, Loadable } from '../loadable'
import { WizardAction } from '../wizard-types'

@Component({
  selector: 'developer-notes',
  templateUrl: './developer-notes.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class DeveloperNotesComponent implements OnInit, Loadable, Colorable {
  @Input() params: {
    action: WizardAction
    developerNotes: string
  }

  $loading$ = new BehaviorSubject(false)
  $color$ = new BehaviorSubject('medium')
  $cancel$ = new Subject<void>()

  load () {}

  constructor () { }
  ngOnInit () {
    switch (this.params.action) {
      case 'install':
        this.summary = `Installation of ${this.params.title} is now in progress. You will receive a notification when the installation has completed.`
        this.label = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title}...`
        this.$color$.next('primary')
        this.successText = 'In Progress'
        break
      case 'downgrade':
        this.summary = `Downgrade for ${this.params.title} is now in progress. You will receive a notification when the downgrade has completed.`
        this.label = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title}...`
        this.$color$.next('primary')
        this.successText = 'In Progress'
        break
      case 'update':
        this.summary = `Update for ${this.params.title} is now in progress. You will receive a notification when the update has completed.`
        this.label = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title}...`
        this.$color$.next('primary')
        this.successText = 'In Progress'
        break
      case 'uninstall':
        this.summary = `${capitalizeFirstLetter(this.params.title)} has been successfully uninstalled.`
        this.label = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title}...`
        this.$color$.next('success')
        this.successText = 'Success'
        break
      case 'stop':
        this.summary = `${capitalizeFirstLetter(this.params.title)} has been successfully stopped.`
        this.label = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title}...`
        this.$color$.next('success')
        this.successText = 'Success'
        break
      case 'configure':
        this.summary = `New config for ${this.params.title} has been successfully saved.`
        this.label = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title}...`
        this.$color$.next('success')
        this.successText = 'Success'
        break
    }
  }
}
