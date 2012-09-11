(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Errors
open Pp
open Util
open Names
open Sign
open Term
open Globnames
open Nametab
open Decl_kinds
open Misctypes
open Locus
open Glob_term

(* Untyped intermediate terms, after ASTs and before constr. *)

let cases_pattern_loc = function
    PatVar(loc,_) -> loc
  | PatCstr(loc,_,_,_) -> loc

let cases_predicate_names tml =
  List.flatten (List.map (function
    | (tm,(na,None)) -> [na]
    | (tm,(na,Some (_,_,nal))) -> na::nal) tml)

let mkGApp loc p t =
  match p with
  | GApp (loc,f,l) -> GApp (loc,f,l@[t])
  | _ -> GApp (loc,p,[t])

let map_glob_decl_left_to_right f (na,k,obd,ty) =
  let comp1 = Option.map f obd in
  let comp2 = f ty in
  (na,k,comp1,comp2)

let map_glob_constr_left_to_right f = function
  | GApp (loc,g,args) ->
      let comp1 = f g in
      let comp2 = Util.list_map_left f args in
      GApp (loc,comp1,comp2)
  | GLambda (loc,na,bk,ty,c) ->
      let comp1 = f ty in
      let comp2 = f c in
      GLambda (loc,na,bk,comp1,comp2)
  | GProd (loc,na,bk,ty,c) ->
      let comp1 = f ty in
      let comp2 = f c in
      GProd (loc,na,bk,comp1,comp2)
  | GLetIn (loc,na,b,c) ->
      let comp1 = f b in
      let comp2 = f c in
      GLetIn (loc,na,comp1,comp2)
  | GCases (loc,sty,rtntypopt,tml,pl) ->
      let comp1 = Option.map f rtntypopt in
      let comp2 = Util.list_map_left (fun (tm,x) -> (f tm,x)) tml in
      let comp3 = Util.list_map_left (fun (loc,idl,p,c) -> (loc,idl,p,f c)) pl in
      GCases (loc,sty,comp1,comp2,comp3)
  | GLetTuple (loc,nal,(na,po),b,c) ->
      let comp1 = Option.map f po in
      let comp2 = f b in
      let comp3 = f c in
      GLetTuple (loc,nal,(na,comp1),comp2,comp3)
  | GIf (loc,c,(na,po),b1,b2) ->
      let comp1 = Option.map f po in
      let comp2 = f b1 in
      let comp3 = f b2 in
      GIf (loc,f c,(na,comp1),comp2,comp3)
  | GRec (loc,fk,idl,bl,tyl,bv) ->
      let comp1 = Array.map (Util.list_map_left (map_glob_decl_left_to_right f)) bl in
      let comp2 = Array.map f tyl in
      let comp3 = Array.map f bv in
      GRec (loc,fk,idl,comp1,comp2,comp3)
  | GCast (loc,c,k) ->
      let comp1 = f c in
      let comp2 = Miscops.map_cast_type f k in
      GCast (loc,comp1,comp2)
  | (GVar _ | GSort _ | GHole _ | GRef _ | GEvar _ | GPatVar _) as x -> x

let map_glob_constr = map_glob_constr_left_to_right

let fold_glob_constr f acc =
  let rec fold acc = function
  | GVar _ -> acc
  | GApp (_,c,args) -> List.fold_left fold (fold acc c) args
  | GLambda (_,_,_,b,c) | GProd (_,_,_,b,c) | GLetIn (_,_,b,c) ->
      fold (fold acc b) c
  | GCases (_,_,rtntypopt,tml,pl) ->
      List.fold_left fold_pattern
	(List.fold_left fold (Option.fold_left fold acc rtntypopt) (List.map fst tml))
	pl
    | GLetTuple (_,_,rtntyp,b,c) ->
	fold (fold (fold_return_type acc rtntyp) b) c
    | GIf (_,c,rtntyp,b1,b2) ->
	fold (fold (fold (fold_return_type acc rtntyp) c) b1) b2
    | GRec (_,_,_,bl,tyl,bv) ->
	let acc = Array.fold_left
	  (List.fold_left (fun acc (na,k,bbd,bty) ->
	    fold (Option.fold_left fold acc bbd) bty)) acc bl in
	Array.fold_left fold (Array.fold_left fold acc tyl) bv
    | GCast (_,c,k) -> fold (match k with CastConv t | CastVM t -> fold acc t | CastCoerce -> acc) c
    | (GSort _ | GHole _ | GRef _ | GEvar _ | GPatVar _) -> acc

  and fold_pattern acc (_,idl,p,c) = fold acc c

  and fold_return_type acc (na,tyopt) = Option.fold_left fold acc tyopt

  in fold acc

let iter_glob_constr f = fold_glob_constr (fun () -> f) ()

let occur_glob_constr id =
  let rec occur = function
    | GVar (loc,id') -> id = id'
    | GApp (loc,f,args) -> (occur f) or (List.exists occur args)
    | GLambda (loc,na,bk,ty,c) -> (occur ty) or ((na <> Name id) & (occur c))
    | GProd (loc,na,bk,ty,c) -> (occur ty) or ((na <> Name id) & (occur c))
    | GLetIn (loc,na,b,c) -> (occur b) or ((na <> Name id) & (occur c))
    | GCases (loc,sty,rtntypopt,tml,pl) ->
	(occur_option rtntypopt)
        or (List.exists (fun (tm,_) -> occur tm) tml)
	or (List.exists occur_pattern pl)
    | GLetTuple (loc,nal,rtntyp,b,c) ->
	occur_return_type rtntyp id
        or (occur b) or (not (List.mem (Name id) nal) & (occur c))
    | GIf (loc,c,rtntyp,b1,b2) ->
	occur_return_type rtntyp id or (occur c) or (occur b1) or (occur b2)
    | GRec (loc,fk,idl,bl,tyl,bv) ->
        not (array_for_all4 (fun fid bl ty bd ->
          let rec occur_fix = function
              [] -> not (occur ty) && (fid=id or not(occur bd))
            | (na,k,bbd,bty)::bl ->
                not (occur bty) &&
                (match bbd with
                    Some bd -> not (occur bd)
                  | _ -> true) &&
                (na=Name id or not(occur_fix bl)) in
          occur_fix bl)
          idl bl tyl bv)
    | GCast (loc,c,k) -> (occur c) or (match k with CastConv t | CastVM t -> occur t | CastCoerce -> false)
    | (GSort _ | GHole _ | GRef _ | GEvar _ | GPatVar _) -> false

  and occur_pattern (loc,idl,p,c) = not (List.mem id idl) & (occur c)

  and occur_option = function None -> false | Some p -> occur p

  and occur_return_type (na,tyopt) id = na <> Name id & occur_option tyopt

  in occur


let add_name_to_ids set na =
  match na with
    | Anonymous -> set
    | Name id -> Idset.add id set

let free_glob_vars  =
  let rec vars bounded vs = function
    | GVar (loc,id') -> if Idset.mem id' bounded then vs else Idset.add id' vs
    | GApp (loc,f,args) -> List.fold_left (vars bounded) vs (f::args)
    | GLambda (loc,na,_,ty,c) | GProd (loc,na,_,ty,c) | GLetIn (loc,na,ty,c) ->
	let vs' = vars bounded vs ty in
	let bounded' = add_name_to_ids bounded na in
       vars bounded' vs' c
    | GCases (loc,sty,rtntypopt,tml,pl) ->
	let vs1 = vars_option bounded vs rtntypopt in
	let vs2 = List.fold_left (fun vs (tm,_) -> vars bounded vs tm) vs1 tml in
	List.fold_left (vars_pattern bounded) vs2 pl
    | GLetTuple (loc,nal,rtntyp,b,c) ->
	let vs1 = vars_return_type bounded vs rtntyp in
	let vs2 = vars bounded vs1 b in
	let bounded' = List.fold_left add_name_to_ids bounded nal in
	vars bounded' vs2 c
    | GIf (loc,c,rtntyp,b1,b2) ->
	let vs1 = vars_return_type bounded vs rtntyp in
	let vs2 = vars bounded vs1 c in
	let vs3 = vars bounded vs2 b1 in
	vars bounded vs3 b2
    | GRec (loc,fk,idl,bl,tyl,bv) ->
	let bounded' = Array.fold_right Idset.add idl bounded in
	let vars_fix i vs fid =
	  let vs1,bounded1 =
	    List.fold_left
	      (fun (vs,bounded) (na,k,bbd,bty) ->
		 let vs' = vars_option bounded vs bbd in
		 let vs'' = vars bounded vs' bty in
		 let bounded' = add_name_to_ids bounded na in
		 (vs'',bounded')
	      )
	      (vs,bounded')
	      bl.(i)
	  in
	  let vs2 = vars bounded1 vs1 tyl.(i) in
	  vars bounded1 vs2 bv.(i)
	in
	array_fold_left_i vars_fix vs idl
    | GCast (loc,c,k) -> let v = vars bounded vs c in
	(match k with CastConv t | CastVM t -> vars bounded v t | _ -> v)
    | (GSort _ | GHole _ | GRef _ | GEvar _ | GPatVar _) -> vs

  and vars_pattern bounded vs (loc,idl,p,c) =
    let bounded' = List.fold_right Idset.add idl bounded  in
    vars bounded' vs c

  and vars_option bounded vs = function None -> vs | Some p -> vars bounded vs p

  and vars_return_type bounded vs (na,tyopt) =
    let bounded' = add_name_to_ids bounded na in
    vars_option bounded' vs tyopt
  in
  fun rt ->
    let vs = vars Idset.empty Idset.empty rt in
    Idset.elements vs


let loc_of_glob_constr = function
  | GRef (loc,_) -> loc
  | GVar (loc,_) -> loc
  | GEvar (loc,_,_) -> loc
  | GPatVar (loc,_) -> loc
  | GApp (loc,_,_) -> loc
  | GLambda (loc,_,_,_,_) -> loc
  | GProd (loc,_,_,_,_) -> loc
  | GLetIn (loc,_,_,_) -> loc
  | GCases (loc,_,_,_,_) -> loc
  | GLetTuple (loc,_,_,_,_) -> loc
  | GIf (loc,_,_,_,_) -> loc
  | GRec (loc,_,_,_,_,_) -> loc
  | GSort (loc,_) -> loc
  | GHole (loc,_) -> loc
  | GCast (loc,_,_) -> loc

(**********************************************************************)
(* Conversion from glob_constr to cases pattern, if possible            *)

let rec cases_pattern_of_glob_constr na = function
  | GVar (loc,id) when na<>Anonymous ->
      (* Unable to manage the presence of both an alias and a variable *)
      raise Not_found
  | GVar (loc,id) -> PatVar (loc,Name id)
  | GHole (loc,_) -> PatVar (loc,na)
  | GRef (loc,ConstructRef cstr) ->
      PatCstr (loc,cstr,[],na)
  | GApp (loc,GRef (_,ConstructRef cstr),l) ->
      PatCstr (loc,cstr,List.map (cases_pattern_of_glob_constr Anonymous) l,na)
  | _ -> raise Not_found

(* Turn a closed cases pattern into a glob_constr *)
let rec glob_constr_of_closed_cases_pattern_aux = function
  | PatCstr (loc,cstr,[],Anonymous) ->
      GRef (loc,ConstructRef cstr)
  | PatCstr (loc,cstr,l,Anonymous) ->
      let ref = GRef (loc,ConstructRef cstr) in
      GApp (loc,ref, List.map glob_constr_of_closed_cases_pattern_aux l)
  | _ -> raise Not_found

let glob_constr_of_closed_cases_pattern = function
  | PatCstr (loc,cstr,l,na) ->
      na,glob_constr_of_closed_cases_pattern_aux (PatCstr (loc,cstr,l,Anonymous))
  | _ ->
      raise Not_found
