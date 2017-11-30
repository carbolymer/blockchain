import {Injectable} from '@angular/core';
import {HttpClient, HttpHeaders} from "@angular/common/http";
import {Transaction} from "./Transaction";
import {Observable} from "rxjs/Observable";
import {catchError, map} from "rxjs/operators";
import {handleError} from "./util";
import {MessageLevel, StatusMessage} from "./StatusMessage";

const httpOptions = {
  headers: new HttpHeaders({'Content-Type': 'application/json'})
};

@Injectable()
export class TransactionService {

  private transactionsUrl = 'api/transactions';

  constructor(private httpService: HttpClient) {
  }

  getTransactions(): Observable<Transaction[]> {
    return this.httpService.get<Transaction[]>(this.transactionsUrl)
      .pipe(
        map(transactions => {
          transactions.forEach(transaction => transaction.time = new Date(transaction.time));
          return transactions.sort((tx1, tx2) => tx2.time.getTime() - tx1.time.getTime());;
        }),
        catchError(handleError('getTransactions', []))
      );
  }

  newTransaction(transaction: Transaction): Observable<StatusMessage> {
    return this.httpService.post<StatusMessage>(`${this.transactionsUrl}/new`, transaction, httpOptions)
      .pipe(
        map((messageDto: any) => {
            return new StatusMessage(
              MessageLevel[(<string>messageDto.level)],
              messageDto.message
            );
          }
        ),
        catchError(handleError<StatusMessage>('newTransaction', {
          level: MessageLevel.ERROR,
          message: 'Could not send request with the new transaction'
        }))
      );
  }
}
