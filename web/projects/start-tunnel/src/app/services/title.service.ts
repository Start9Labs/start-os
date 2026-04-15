import { inject, Injectable } from '@angular/core'
import { Title } from '@angular/platform-browser'
import { RouterStateSnapshot, TitleStrategy } from '@angular/router'

@Injectable()
export class AppTitleStrategy extends TitleStrategy {
  private readonly title = inject(Title)

  override updateTitle(snapshot: RouterStateSnapshot): void {
    this.title.setTitle(`StartTunnel – ${this.buildTitle(snapshot)}`)
  }
}
