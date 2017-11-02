import {Injectable} from '@angular/core';
import {handleError} from "./util";
import {catchError} from "rxjs/operators";
import {Observable} from "rxjs/Observable";
import {HttpClient} from "@angular/common/http";
import {Account} from "./Account"

@Injectable()
export class AccountService {

  private accountsUrl = 'api/accounts';

  constructor(private httpService: HttpClient) {}

  getAccounts(): Observable<Account[]> {
    return this.httpService.get<Account[]>(this.accountsUrl)
      .pipe(
        catchError(handleError('getAccounts', []))
      );
  }
}
