import { Injectable } from '@angular/core'
import { Dump } from 'patch-db-client'
import { TunnelData } from '../patch-db/data-model'
import { Observable } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export abstract class ApiService {
  abstract openWebsocket$<T>(guid: string): Observable<T>
  abstract subscribe(): Promise<SubscribeRes>
  // auth
  abstract login(params: LoginReq): Promise<null>
  abstract logout(): Promise<null>
  abstract setPassword(params: LoginReq): Promise<null>
  // subnets
  abstract addSubnet(params: UpsertSubnetReq): Promise<null>
  abstract editSubnet(params: UpsertSubnetReq): Promise<null>
  abstract deleteSubnet(params: DeleteSubnetReq): Promise<null>
  // devices
  abstract addDevice(params: UpsertDeviceReq): Promise<null>
  abstract editDevice(params: UpsertDeviceReq): Promise<null>
  abstract deleteDevice(params: DeleteDeviceReq): Promise<null>
  // forwards
  abstract addForward(params: AddForwardReq): Promise<null>
  abstract deleteForward(params: DeleteForwardReq): Promise<null>
}

export type SubscribeRes = {
  dump: Dump<TunnelData>
  guid: string
}

export type LoginReq = { password: string }

export type UpsertSubnetReq = {
  name: string
  subnet: string
}

export type DeleteSubnetReq = {
  subnet: string
}

export type UpsertDeviceReq = {
  name: string
  subnet: string
  ip: string
}

export type DeleteDeviceReq = {
  subnet: string
  ip: string
}

export type AddForwardReq = {
  source: string // externalip:port
  target: string // internalip:port
}

export type DeleteForwardReq = {
  source: string
}
