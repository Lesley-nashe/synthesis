module Synthesis

let abelar input =
   match (12<input) && (input%12 = 0) && (input<3097) with
   | true -> true
   | false -> false
  

let area bayse height =
   match (bayse<0.0) || (height<0.0) with
    |false -> 0.5 * bayse * height
    |true -> failwith "Throw an exception"
   

let zollo h =
   match h<0 with
    | true -> h * -1 
    | false -> h* 2

let min a b =
   match a>b with
    |true -> b
    |false -> a
    

let max a b =
   match a>b with
    | true -> a
    |false -> b
    

let ofTime hours minutes seconds = (hours*3600)+(minutes*60)+(seconds)

   
    
    

let toTime theSeconds = 
 let hours = (theSeconds/3600)
 let minutes = (theSeconds%3600)/60
 let seconds = theSeconds%60
 match theSeconds>0 with
  | true -> hours,minutes,seconds
  | _ -> 0,0,0





   

let digits number =
  let rec num a count=
    match a = 0 with
    | true -> count
    | _ -> num(a/10) (count+1)
  match number = 0 with
  |true -> 1
  |_ -> num number 0

    
let minmax (num1, num2, num3, num4) =
    let maxmax = max (max num1 num2) (max num3 num4)
    let minmin = min (min num1 num2) (min num3 num4)
    (minmin,maxmax)
    

let isLeap year =
  match year>=1582 with
  |false ->failwith"Cannot happen"
  |true -> 
  match (year%4 =0)&&(year%400=0)&&(year%100 = 0)||(year%4 = 0 && year%100<>0 ) with
  |true -> true
  |false -> false

    

let month num =
  match num  with
   | 1 -> "January", 31
   | 2 -> "February", 28
   | 3 -> "March", 31
   | 4 -> "April", 30
   | 5 -> "May", 31
   | 6 -> "June", 30
   | 7 -> "July", 31
   | 8 -> "August", 31
   | 9 -> "September", 30
   | 10 ->"October", 31
   | 11 -> "November", 30
   | 12 -> "December", 31
   | anythingelse -> failwith "That's not a Month"
   
let toBinary number =
    failwith "Not implemented"

let bizFuzz num =
   failwith "Not implemented"
  
    

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"