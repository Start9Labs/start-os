import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TuiAppearance, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCardLarge, TuiCell, TuiHeader } from '@taiga-ui/layout'
import { StateService } from '../services/state.service'

@Component({
  template: `
    <div tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>{{ 'Select Setup Flow' | i18n }}</h2>
      </header>

      <button tuiCell="l" (click)="startFresh()">
        <tui-avatar appearance="positive" src="@tui.plus" />
        <div tuiTitle>
          {{ 'Start Fresh' | i18n }}
          <div tuiSubtitle>{{ 'Set up a brand new server' | i18n }}</div>
        </div>
      </button>

      <button tuiCell="l" (click)="restore()">
        <tui-avatar appearance="warning" src="@tui.archive-restore" />
        <div tuiTitle>
          {{ 'Restore from Backup' | i18n }}
          <div tuiSubtitle>
            {{ 'Restore StartOS data from an encrypted backup' | i18n }}
          </div>
        </div>
      </button>

      <button tuiCell="l" (click)="transfer()">
        <tui-avatar appearance="info" src="@tui.hard-drive-download" />
        <div tuiTitle>
          {{ 'Transfer' | i18n }}
          <div tuiSubtitle>
            {{ 'Transfer data from an existing StartOS data drive' | i18n }}
          </div>
        </div>
      </button>
    </div>
  `,
  imports: [
    TuiAppearance,
    TuiCardLarge,
    TuiHeader,
    TuiCell,
    TuiTitle,
    TuiAvatar,
    i18nPipe,
  ],
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
