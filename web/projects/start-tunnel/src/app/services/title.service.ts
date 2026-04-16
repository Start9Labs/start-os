import { inject, Injectable } from '@angular/core'
import { Title } from '@angular/platform-browser'
import { RouterStateSnapshot, TitleStrategy } from '@angular/router'

@Injectable()
export class AppTitleStrategy extends TitleStrategy {
  private readonly title = inject(Title)

  override updateTitle(snapshot: RouterStateSnapshot): void {
    this.title.setTitle(this.parse(this.buildTitle(snapshot)))
  }

  private parse(title?: string): string {
    return title ? `StartTunnel – ${title}` : 'StartTunnel'
  }
}
