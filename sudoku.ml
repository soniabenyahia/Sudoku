(* Question 1 *)


(*Declaration du type grille comme tableau à deux dimensions*)
 
  type grille =int array array ;;
  (*let (g1:grille) = Array.make_matrix 9 9 0 ;;*)

(*remplir une grille valide*)
(*let k = ref 0;;
  for i=0 to 8 do 
    for j=0 to 8 do 
      g1.(i).(j) <- (!k) ;
      k:= !k+1 ;
    done;
  done;;*)


  let (g:grille)=
  [|[|5;3;4;6;7;8;9;1;2|]; [|6;7;2;1;9;5;3;4;8|];
    [|1;9;8;3;4;2;5;6;7|]; [|8;5;9;7;6;1;4;2;3|];
    [|4;2;6;8;5;3;7;9;1|]; [|7;1;3;9;2;4;8;5;6|];
    [|9;6;1;5;3;7;2;8;4|]; [|2;8;7;4;1;9;6;3;5|];
    [|3;4;5;2;8;6;1;7;9|]|] ;;



(* fct  afficher_grille : afficher une grille *)

  let afficher_grille grille =
    for i = 0 to Array.length grille -1 do
        for j = 0 to Array.length grille.(0) - 1 do
            print_int grille.(i).(j);
            let mot = if ((j+1) mod (Array.length grille.(0))) = 0 then "\n" else "\t" in
            print_string mot;
            done;
        done;
    print_string "\n \n";;



(* fct case:renvoyer le contenu de la case(i,j) de la grille g  *)

let  case  (g : grille )  (i:int) (j:int)  = g.(i).(j);;

case  g 1 1;;
afficher_grille g;;

(* fct verifier_ligne:verifier si toutes les lignes sont valides*)

let  verifier_ligne  (g:grille) =try
   for i = 0 to Array.length g.(0) - 1 do
       for j = 0 to Array.length g.(0) - 1 do
              for z= j+1 to Array.length g.(0) - 1 do
       if g.(i).(j)==g.(i).(z) then raise Exit  done
       done
    done;
    true
    with
    Exit -> false ;;


(* fct verifier_colonne: verifier si toutes les colonnes sont valides *)

let  verifier_colonne (g:grille) =try
    for c = 0 to Array.length g.(0) - 1 do
       for i = 0 to Array.length g.(0) - 1 do
              for j= i+1 to Array.length g.(0) - 1 do
       if g.(j).(c)==g.(i).(c) then raise Exit  done
       done
    done;
    true
    with
    Exit -> false;;



(* fct verifier_bloc:verifier si tous les blocs sont valides *)

let verifier_bloc (g:grille) = try
 for x=0 to 8 do 
   if(x=0 || x=3 || x=6) then 
     for y=0 to 8 do 
      if(y=0 || y=3 || y=6) then 
         for l=x to x+2 do 
           for c=y to y+2 do 
              let a = g.(l).(c) in
              for i=x to x+2 do 
                for j=y to y+2 do 
                  if(g.(i).(j)== a && (i<>l || j<>c)) then raise  Exit  done
              done ;
           done;
         done;
    done;  
  done;
  true          
  with
  Exit -> false;;

  

(*  fct test:tester si la grille g est une solution de sudoku valide *)

let test (g:grille)=if ((verifier_bloc g) && (verifier_ligne g) &&  (verifier_colonne g))then true else false ;;
test g ;;

(*Declaration du type probleme comme tableau à deux dimensions*)
type probleme =int array array ;;

let (p:probleme)=
  [|[|5;3;0;0;7;0;0;0;0|]; [|6;0;0;1;9;5;0;0;0|];
    [|0;9;8;0;0;0;0;6;0|]; [|8;0;0;0;6;0;0;0;3|];
    [|4;0;0;8;0;3;0;0;1|]; [|7;0;0;0;2;0;0;0;6|];
    [|0;6;0;0;0;0;2;8;4|]; [|0;0;0;4;1;9;0;0;5|];
    [|0;0;0;0;8;0;0;7;9|]|] ;;
 afficher_grille p;;
 let solution_de (p:probleme) (g:grille)= try
    for i=0 to 8 do
        for j=0 to 8 do
        if((test g)==true) then 
           if p.(i).(j)<> g.(i).(j) && p.(i).(j)<>0 then raise Exit done
           done;
              true
              with
            Exit -> false;;


solution_de p g ;;

type hypothese = int array array array;;
let (h:hypothese)= Array.make_matrix 9 9 [|0|];;

h.(0).(1)<-[|0;3|];;
h.(0).(8)<-[|5;2;3|];;

h;;

let compatible (h:hypothese) (g:grille) = let x = ref 0 in
    try
        for i = 0 to 8 do
            for j = 0 to 8 do

            if h.(i).(j) <> [|0|]
            then 
            (
                    x := 0;
            for z = 0 to (Array.length h.(i).(j))-1 do
        
                if (h.(i).(j).(z) == g.(i).(j))
                then x := !x + 1;


            done;
            if  !x = 0
            then raise Exit
            else x := 0 
            )
        done;
        done;
        true;

    with Exit -> false;;

compatible h g ;;
(*let take (h:hypothese) (g:grille) x y =*)
