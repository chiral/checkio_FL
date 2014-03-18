#light

let checkio (text:string,words:string) =
  let lt = text.ToLower()
  let lw = words.ToLower()
  let range = [0..text.Length-1]
  let index w =
    (fun i -> lt.Substring(i).StartsWith(w))
    |> List.filter <| range
    |> List.map (fun i -> (i,i+w.Length))
  let span =
    lw.Split([|' '|]) |> Seq.map index |> Seq.concat
  let spans =
    let hits i = Seq.filter (fun (j,k)->j<=i && i<k) span
    List.map (Set.ofSeq << hits) range |> Array.ofList
  let spanGet j =
    if j>=0 && j<text.Length then spans.[j] else Set.empty
  let border x y =
    not (Set.isEmpty x) && Set.isEmpty (Set.intersect x y)
  let tag i j t =
    if border (spanGet i) (spanGet j) then t else ""
  let trans (i,c) =
    (tag i (i-1) "<span>") + c.ToString() + (tag i (i+1) "</span>")
  Seq.zip range text |> Seq.map trans |> String.concat ""

let my_assert info args expected =
  let ans = checkio args
  if ans <> expected then
    printf "Failed : %A\n%A\n" info ans

let my_main args = 
  
  my_assert 1 ("This is only a text example for task example.","example")
    "This is only a text <span>example</span> for task <span>example</span>."
    
  my_assert 2 ("Python is a widely used high-level programming language.", "pyThoN")
    "<span>Python</span> is a widely used high-level programming language."

  my_assert 3 ("It is experiment for control groups with similar distributions.", "is im")
    ("It <span>is</span> exper<span>im</span>ent"+
     " for control groups with s<span>im</span>ilar d<span>is</span>tributions.")

  my_assert 4 ("The National Aeronautics and Space Administration (NASA).", "nasa  THE")
    "<span>The</span> National Aeronautics and Space Administration (<span>NASA</span>)."

  my_assert 5 ("Did you find anything?", "word space tree")
    "Did you find anything?"

  my_assert 6 ("Hello World! Or LOL", "hell world or lo")
    "<span>Hello</span> <span>World</span>! <span>Or</span> <span>LO</span>L"
                     
  printfn "done."; 0

#if INTERACTIVE
my_main ()
#endif

#if COMPILED
[<EntryPoint>]
let main args = my_main args
#endif
