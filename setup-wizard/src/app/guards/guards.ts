import { Injectable } from '@angular/core'
import { CanActivate } from '@angular/router'
import { StateService } from '../services/state.service'

@Injectable({
  providedIn: 'root',
})
export class CanActivateHome implements CanActivate {

  constructor (
    private readonly stateService: StateService
  ) {}

  canActivate (): boolean {
    console.log(!!this.stateService.recoveryDrive)
    return !!this.stateService.recoveryDrive ? false : true
  }
}

@Injectable({
  providedIn: 'root',
})
export class CanActivateRecover implements CanActivate {

  constructor (
    private readonly stateService: StateService
  ) {}

  canActivate (): boolean {
    return this.stateService.dataDrive ? true : false
  }
}
