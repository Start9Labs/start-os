import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TuiTitle, TuiCell } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { StateService } from '../services/state.service'

@Component({
  template: `
    <div tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>{{ 'Select Setup Flow' | i18n }}</h2>
      </header>

      <button tuiCell="l" (click)="startFresh()">
        <span tuiAvatar="@tui.plus" appearance="positive"></span>
        <div tuiTitle>
          <b>{{ 'Start Fresh' | i18n }}</b>
          <div tuiSubtitle>{{ 'Set up a brand new server' | i18n }}</div>
        </div>
      </button>

      <button tuiCell="l" (click)="restore()">
        <span tuiAvatar="@tui.archive-restore" appearance="warning"></span>
        <div tuiTitle>
          <b>{{ 'Restore from Backup' | i18n }}</b>
          <div tuiSubtitle>
            {{ 'Restore StartOS data from an encrypted backup' | i18n }}
          </div>
        </div>
      </button>

      <button tuiCell="l" (click)="transfer()">
        <span tuiAvatar="@tui.hard-drive-download" appearance="info"></span>
        <div tuiTitle>
          <b>{{ 'Transfer' | i18n }}</b>
          <div tuiSubtitle>
            {{ 'Transfer data from an existing StartOS data drive' | i18n }}
          </div>
        </div>
      </button>
    </div>
  `,
  imports: [TuiCardLarge, TuiHeader, TuiCell, TuiTitle, TuiAvatar, i18nPipe],
})
export default class HomePage {
  private readonly router = inject(Router)
  private readonly stateService = inject(StateService)

  async startFresh() {
    this.stateService.setupType = 'fresh'
    this.stateService.recoverySource = undefined
    await this.router.navigate(['/password'])
  }

  async restore() {
    this.stateService.setupType = 'restore'
    await this.router.navigate(['/restore'])
  }

  async transfer() {
    this.stateService.setupType = 'transfer'
    await this.router.navigate(['/transfer'])
  }
}
