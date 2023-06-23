

export class CallbackHolder {
    constructor() {

    }
    private callbacks: Function[] = []

    addCallback(callback: Function) {
        return this.callbacks.push(callback) - 1
    }
    callCallback(index: number, args: any[]): Promise<unknown> {
        return Promise.resolve().then(() => this.callbacks[index](...args))
    }
}