export class Transaction {
  constructor(public amount: number,
              public sender: string,
              public recipient: string,
              public time: Date) {
  }

  static fromTransactionDto(dto): Transaction {
    return new Transaction(
      dto._operation._amount,
      dto._operation.tag === 'Reward' ? '0' : dto._operation._sender,
      dto._operation._recipient,
      new Date(dto._operation._time)
    );
  }
}

export class TransactionHistoryEntry {
  constructor(public otherAddress: string,
              public amount: number,
              public balance: number,
              public time: Date) {
  }
}


export class NewTransactionRequest {
  constructor(public newAmount: number,
              public newSender: string,
              public newRecipient: string) {
  }

}
