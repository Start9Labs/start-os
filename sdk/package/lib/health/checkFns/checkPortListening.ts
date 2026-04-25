import { Effects } from '../../../../base/lib/types'
import { HealthCheckResult } from './HealthCheckResult'
import * as fs from 'node:fs/promises'

export function containsAddress(x: string, port: number, address?: bigint) {
  const readPorts = x
    .split('\n')
    .filter(Boolean)
    .splice(1)
    .map((x) => x.split(' ').filter(Boolean)[1]?.split(':'))
    .filter((x) => x?.length > 1)
    .map(([addr, p]) => [BigInt(`0x${addr}`), Number.parseInt(p, 16)] as const)
  return !!readPorts.find(
    ([addr, p]) => (address === undefined || address === addr) && port === p,
  )
}

/**
 * Check whether a TCP or UDP port is currently bound and listening.
 *
 * Reads `/proc/net/tcp{,6}` and `/proc/net/udp{,6}` to determine if any
 * socket is bound to the given port. This is a lightweight, non-intrusive
 * check — it does not open a connection or send any data.
 *
 * @param effects - The effects context
 * @param port - Port number to check
 * @param options.successMessage - Message shown in the UI when the port is listening
 * @param options.errorMessage - Message shown in the UI when the port is not listening
 * @param options.timeoutMessage - Message shown if the check exceeds the timeout (defaults to `"Timeout trying to check port {port}"`)
 * @param options.timeout - Maximum time (ms) to wait before reporting failure (defaults to 1000)
 */
export async function checkPortListening(
  effects: Effects,
  port: number,
  options: {
    errorMessage: string
    successMessage: string
    timeoutMessage?: string
    timeout?: number
  },
): Promise<HealthCheckResult> {
  return Promise.race<HealthCheckResult>([
    Promise.resolve().then(async () => {
      const hasAddress =
        containsAddress(await fs.readFile('/proc/net/tcp', 'utf-8'), port) ||
        containsAddress(
          await fs.readFile('/proc/net/tcp6', 'utf-8'),
          port,
          BigInt(0),
        ) ||
        containsAddress(await fs.readFile('/proc/net/udp', 'utf-8'), port) ||
        containsAddress(
          await fs.readFile('/proc/net/udp6', 'utf-8'),
          port,
          BigInt(0),
        )
      if (hasAddress) {
        return { result: 'success', message: options.successMessage }
      }
      return {
        result: 'failure',
        message: options.errorMessage,
      }
    }),
    new Promise((resolve) => {
      setTimeout(
        () =>
          resolve({
            result: 'failure',
            message:
              options.timeoutMessage || `Timeout trying to check port ${port}`,
          }),
        options.timeout ?? 1_000,
      )
    }),
  ])
}
