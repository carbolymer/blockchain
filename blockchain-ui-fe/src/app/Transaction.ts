export class Transaction {
  constructor(public amount: number,
              public sender: string,
              public recipient: string,
              public time: Date) {
  }
}

export class TransactionHistoryEntry {
  constructor(public otherAddress: string,
              public amount: number,
              public balance: number,
              public time: Date) {
  }
}
