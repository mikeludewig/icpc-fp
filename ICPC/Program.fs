module ICPC
open System

let CheckBeginningANDEnd (inputString:string) = 
    match (inputString.Substring(inputString.Length-1)) = "." || (inputString.Substring(inputString.Length-1)) = "," || (inputString.Substring(inputString.Length-1)) = " " || (inputString.Substring(inputString.Length-inputString.Length+1)) = "." || (inputString.Substring(inputString.Length-inputString.Length+1)) = "," || (inputString.Substring(inputString.Length-inputString.Length+1)) = " " with
    |true -> true
    |false -> false

let CheckCharacters (input: string)=
    let CharList3 = Seq.toList input //converts a string to a list of characters
    let rec CharITER list =
        match list with 
        |[] -> false
        |cchar::rest -> match Char.IsWhiteSpace(cchar) || Char.IsLetter(cchar) || cchar = ',' || cchar = '.' with
                        |true -> CharITER rest
                        |false -> true
    CharITER CharList3

let CheckCapitalLetters (input: string) = 
    let charList = Seq.toList input
    let rec CapitalSearch [] = 
        match [] with
        |[] -> false
        | cchar::rest -> match Char.IsUpper(cchar) with | true -> true | false -> CapitalSearch rest
    CapitalSearch charList

let CheckForMoreThanOneWord (input: string) =
    let charList2 = Seq.toList input
    let rec SpaceSearch list = 
        match list with
        |[] -> true
        |cchar::rest -> match cchar = ' '  with | true -> false | false -> SpaceSearch rest //Char.IsWhiteSpace(cchar)
    SpaceSearch charList2

let rec PostComma (input: string list) PostCommaWordList =
    match input with
    |[] -> PostCommaWordList
    |word::rest -> match word.Substring(word.Length-1) = "," with
                    |true -> PostComma rest (word::PostCommaWordList)
                    |false -> PostComma rest PostCommaWordList
    
let rec PreComma (input: string list) PreCommaWordList previous=
    //let charList = Seq.toList input
    match input with
    |[] -> PreCommaWordList
    |word::rest -> match word.Substring(word.Length-1) = "," with
                    |true -> PreComma rest (previous::PreCommaWordList) word
                    |false -> PreComma rest PreCommaWordList word

let rec PostCommaRules (input:string list) (AddComma:string) =
    match input with
    |[] -> input
    |word::rest -> match word = AddComma with
                    |true -> match word.Substring(word.Length-1) = "," || word.Substring(word.Length-1) = "." with
                            |true -> PostCommaRules rest AddComma //no need to add comma case
                            |false -> word + ","
                                      PostCommaRules rest AddComma
                    |false -> PostCommaRules rest AddComma

let rec PreCommaRules (input:string list) (AddComma:string) (PreviousWord:string)= 
    match input with
    |[] -> input
    |word::rest -> match word = AddComma with
                    |true -> match PreviousWord.Substring(PreviousWord.Length-1) = "," || PreviousWord.Substring(PreviousWord.Length-1) = "." with
                            |true -> PreCommaRules rest AddComma PreviousWord
                            |false -> PreviousWord + ","
                                      PreCommaRules rest AddComma PreviousWord
                    |false -> PreCommaRules rest AddComma PreviousWord

let rec CommaAdder (input:string list) (PostCommaWordList: string list) =
    match PostCommaWordList with
    |[] -> input
    |word::rest -> PostCommaRules input word

let rec PostCommaAdder (input:string list) (PreCommaWordList: string list) = 
    match PreCommaWordList with
    |[] -> input

let commaSprinkler input =
   /// failwith "Not implemented"
   match (CheckForMoreThanOneWord (input)) || (String.length(input) < 2) || (CheckBeginningANDEnd (input))  || (CheckCapitalLetters (input)) || ( CheckCharacters (input)) with
   |true -> None
   |false -> failwith "not implemented" 
            //let WordList = input.Split[|' '|]
            //let PostCommaWords = PostComma (WordList) []
            //let charList = Seq.toList input
            //let PreCommaWords = PreComma (WordList) [] ""
            //let newList = CommaAdder input PostCommaWords


   //let lowercase = input.toLower()//failwith "not implemented"
            //let Lyst = lowercase.split[|' '|]//string.Split(input)
            //let LystLength = Lyst.length()
        //  let rec Lyst 

let MultipleContiguousSpaces (input:string) =
    let charLIst = Seq.toList input
    let rec Iterate list previous =
        match list with
        |[] -> false
        |cchar::rest -> match cchar = ' ' with
                        |false -> Iterate rest cchar
                        |true -> match cchar = previous with
                                |true -> true
                                |false -> Iterate rest cchar
    let hd::rest = charLIst
    Iterate charLIst hd

let CheckSpacePosition (input:string) =
    let wordList = input.Split [|' '|]
    match (input.Substring(input.Length-1) = " ") || (input.Substring((input.Length - input.Length)+1) = " ") || wordList.Length < 2  with
    |true -> true
    |false -> false


let CheckWordLength (input:string) =
    let charLIst2 = Seq.toList input
    let rec Iterate list charcount =
        match list with
        |[] -> false
        |cchar::rest -> match cchar = ' ' with
                        |true -> match charcount > 80 with
                                |true -> true
                                |false -> Iterate rest 0
                        |false -> Iterate rest (charcount+1)
    Iterate charLIst2 0

   
    
   // let rec iter list =
   //     match list with
   //     |[] -> true
   //     |word::rest -> match word.Length() > 80 with
   //                     |true -> false
   //                     |false -> iter rest
   // iter wordList
   // *)
let Checktextcontent (input: string)=
    let CharList3 = Seq.toList input
    let rec CharITER list =
        match list with 
        |[] -> false
        |cchar::rest -> match Char.IsWhiteSpace(cchar) || Char.IsLetter(cchar) with
                        |true -> CharITER rest
                        |false -> true
    CharITER CharList3

let rivers (input:string) =
    //let wordList = input.Split[|' '|]
    match input = "" || CheckSpacePosition(input) || Checktextcontent(input) || MultipleContiguousSpaces(input) || CheckWordLength(input)  with//wordList.Length <2 with //|| CheckSpacePosition(input) with//CheckWordLength(input) with //failwith "Not implemented"
    |true -> None
    |false-> failwith "not implememnted"
            

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
