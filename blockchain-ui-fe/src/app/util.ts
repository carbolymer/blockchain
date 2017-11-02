import {Observable} from "rxjs/Observable";
import {of} from "rxjs/observable/of";

export function handleError<T>(operation: string = 'operation', result?: T) {
  return (error: any): Observable<T> => {
    console.error(`${operation} failed: ${error.message}`);
    console.error(error);
    return of(result as T);
  }
}

interface Array<T> {
  flatMap<E>(callback: (t: T) => Array<E>): Array<E>
}

Object.defineProperty(Array.prototype, 'flatMap', {
  value: function (f: Function) {
    return this.reduce((ys: any, x: any) => {
      return ys.concat(f.call(this, x))
    }, [])
  },
  enumerable: false,
});
